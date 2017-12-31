package leelazero.network

import jcuda.driver.JCudaDriver._
import jcuda.driver._
import leelazero.util.Utils.myPrint
import leelazero.network.OpenCL._
import leelazero.Config
import java.nio.charset.Charset
import Network.FLOAT_SIZE
import org.jocl.CL._
import org.jocl.{cl_context_properties, _}


object OpenCL {

  val sourceCode_config: String = """(
      |    #ifdef USE_HALF
      |    typedef half net_t;
      |    #define vload_net_t(offset,p) vload_half(offset,p)
      |    #define vstore_net_t(data,offset,p) vstore_half(data,offset,p)
      |    #else
      |    typedef float net_t;
      |    #define vload_net_t(offset,p) ((p)[(offset)])
      |    #define vstore_net_t(data,offset,p) (((p)[(offset)])=(data))
      |    #endif
      |)"""

  val sourceCode_convolve1: String = """(
      |    __kernel
      |    __attribute__((work_group_size_hint(8, 16, 1)))
      |    void convolve1(
      |                   __global const net_t * in,
      |                   __global net_t * merge,
      |                   __global const net_t * weights,
      |                   __local float * channel_buff,
      |                   __local float * row_buff) {
      |        // cl::NDRange global(channels, outputs, row);
      |        const int c   = get_global_id(0);  // channel
      |        const int o   = get_global_id(1);  // output
      |        const int row = get_global_id(2);  // row
      |
      |        const int channels = get_global_size(0);
      |        const int outputs  = get_global_size(1);
      |
      |        // cl::NDRange local(2, (1->32), 1);
      |        const int lx = get_local_id(0);
      |        const int ly = get_local_id(1);
      |
      |        const int chan_buff_size = 8;
      |        const int out_buff_size  = get_local_size(1);
      |        const int row_buff_size  = 7;
      |        const int chan_shift     = 3;
      |
      |        // input = channels * height * width
      |        // output = outputs * height * width
      |        // weights = output * channels * filter
      |        // merge = channels * outputs * height * width
      |
      |        const int width = 19;
      |        const int height = 19;
      |        const int strip_size = width;
      |
      |        // Copy the input channels (strips) locally
      |        if (out_buff_size < 19 && ly == 0) {
      |            // strip-row
      |            for (int w = 0; w < width; w++) {
      |                channel_buff[lx * width + w] =
      |                    vload_net_t((c * height + row) * width + w, in);
      |            }
      |        } else if (out_buff_size >= 19 && ly < 19) {
      |            // Every thread copies a column
      |            channel_buff[lx * width + ly] = vload_net_t((c * height + row) * width + ly, in);
      |        }
      |
      |        // Copy the filter we are applying locally
      |        __private float filter_buff = vload_net_t((o * channels + c), weights);
      |
      |        barrier(CLK_LOCAL_MEM_FENCE);
      |
      |        int out_lane = 0;
      |        int out_cw   = 0;
      |        #pragma unroll
      |        for (int cw = 0; cw < width; cw++) {
      |            int fid = lx * strip_size;
      |            float out  = channel_buff[fid + cw] * filter_buff;
      |            row_buff[(ly * chan_buff_size + lx) * row_buff_size + out_lane] = out;
      |            out_lane++;
      |            // Row buffer full or last lane?
      |            if (out_lane == row_buff_size || (cw == width - 1)) {
      |                barrier(CLK_LOCAL_MEM_FENCE);
      |                if (lx < out_lane) {
      |                    float val;
      |                    val  = row_buff[(ly * chan_buff_size + 0) * row_buff_size + lx];
      |                    val += row_buff[(ly * chan_buff_size + 1) * row_buff_size + lx];
      |                    val += row_buff[(ly * chan_buff_size + 2) * row_buff_size + lx];
      |                    val += row_buff[(ly * chan_buff_size + 3) * row_buff_size + lx];
      |                    val += row_buff[(ly * chan_buff_size + 4) * row_buff_size + lx];
      |                    val += row_buff[(ly * chan_buff_size + 5) * row_buff_size + lx];
      |                    val += row_buff[(ly * chan_buff_size + 6) * row_buff_size + lx];
      |                    val += row_buff[(ly * chan_buff_size + 7) * row_buff_size + lx];
      |                    vstore_net_t(val, (((c >> chan_shift) * height + row) * width + out_cw + lx) * outputs + o, merge);
      |                }
      |                out_cw  += row_buff_size;
      |                out_lane = 0;
      |           }
      |       }
      |    }
      |)""".stripMargin

    val sourceCode_convolve3: String = """(
      |    __kernel
      |    __attribute__((work_group_size_hint(8, 32, 1)))
      |    void convolve3(
      |                   __global const net_t * in,
      |                   __global net_t * merge,
      |                   __global const net_t * weights,
      |                   __local float * channel_buff,
      |                   __local float * row_buff,
      |                   const int row_tile_size,
      |                   const int row_buff_size,
      |                   const int chan_buff_size,
      |                   const int chan_shift) {
      |
      |        // cl::NDRange global(channels, outputs, row);
      |        const int c   = get_global_id(0);  // channel
      |        const int o   = get_global_id(1);  // output
      |        const int r   = get_global_id(2);  // row
      |
      |        const int channels = get_global_size(0);
      |        const int outputs  = get_global_size(1);
      |
      |        // cl::NDRange local(2, (1->32), 1);
      |        const int lx = get_local_id(0);
      |        const int ly = get_local_id(1);
      |
      |        const int out_buff_size  = get_local_size(1);
      |        const int width = 19;
      |        const int height = 19;
      |
      |        const int filter_size = 3;
      |        const int filter_len = filter_size * filter_size;
      |        const int mid = (filter_size / 2) + 1;
      |        const int extent = mid - 1;
      |        const int pad_width = width + filter_size - 1;
      |
      |        // input = channels * height * width
      |        // output = outputs * height * width
      |        // weights = output * channels * filter
      |        // merge = channels * outputs * height * width
      |
      |        __private float filter_buff[9];
      |        __private float chan_cache[2];
      |        __private float stripe_cache[9];
      |
      |        // Copy the filter we are applying locally
      |        // output * channel * filter_len
      |        for (int f = 0; f < filter_len; f++) {
      |            filter_buff[f] = vload_net_t((o * channels + c) * filter_len + f, weights);
      |        }
      |
      |        for (int tile = 0; tile < row_tile_size; tile++) {
      |            int row = r * row_tile_size + tile;
      |            if (row > 18) break;
      |
      |            // Copy the input channels (strips) locally
      |            if (out_buff_size < 21 && ly == 0) {
      |                // strip-row
      |                for (int srow = 0; srow < filter_size; srow++) {
      |                    int in_row = row - extent + srow;
      |                    channel_buff[(lx * pad_width + 0) * filter_size + srow]             = 0.0f;
      |                    if ((unsigned)in_row < height) {
      |                        for (int w = 0; w < width; w++) {
      |                            float val = vload_net_t((c * height + in_row) * width + w, in);
      |                            channel_buff[(lx * pad_width + w + extent) * filter_size + srow] = val;
      |                        }
      |                    } else {
      |                        for (int w = 0; w < width; w++) {
      |                            channel_buff[(lx * pad_width + w + extent) * filter_size + srow] = 0.0f;
      |                        }
      |                    }
      |                    channel_buff[(lx * pad_width + pad_width - 1) * filter_size + srow] = 0.0f;
      |                }
      |            } else if (out_buff_size >= 21 && ly < 21) {
      |                // Every thread copies a column
      |                int copy_idx = (lx * pad_width + ly) * filter_size;
      |                if (tile == 0 || row == 18) {
      |                    // Every thread copies a column
      |                    for (int srow = 0; srow < filter_size; srow++) {
      |                        int in_row = row - extent + srow;
      |                        float val = 0.0f;
      |                        if ((unsigned)in_row < height && ly >= 1 && ly <= 19) {
      |                            val = vload_net_t((c * height + in_row) * width + ly - 1, in);
      |                        }
      |                        channel_buff[copy_idx + srow] = val;
      |                        if (srow > 0) {
      |                            chan_cache[srow - 1] = val;
      |                        }
      |                    }
      |                } else {
      |                    int in_row = row - extent + 2;
      |                    float val = 0.0f;
      |                    if (ly >= 1 && ly <= 19) {
      |                        val = vload_net_t((c * height + in_row) * width + ly - 1, in);
      |                    }
      |                    channel_buff[copy_idx + 0] = chan_cache[0];
      |                    channel_buff[copy_idx + 1] = chan_cache[1];
      |                    channel_buff[copy_idx + 2] = val;
      |                    chan_cache[0] = chan_cache[1];
      |                    chan_cache[1] = val;
      |                }
      |            }
      |
      |            int out_lane = 0;
      |            int out_cw   = 0;
      |            __local float * out_row_buff = &row_buff[(ly * chan_buff_size + lx) * row_buff_size];
      |            int fid = (lx * pad_width) * filter_size;
      |            barrier(CLK_LOCAL_MEM_FENCE);
      |
      |            for (int rc = 0; rc < 9; rc++) {
      |                stripe_cache[rc] = channel_buff[fid + rc];
      |            }
      |
      |            #pragma unroll
      |            for (int cw = 0; cw < width; cw++) {
      |                // Start filter
      |                float out  =   stripe_cache[      0] * filter_buff[0]
      |                             + stripe_cache[      1] * filter_buff[3]
      |                             + stripe_cache[      2] * filter_buff[6]
      |                             + stripe_cache[      3] * filter_buff[1]
      |                             + stripe_cache[      4] * filter_buff[4]
      |                             + stripe_cache[      5] * filter_buff[7]
      |                             + stripe_cache[      6] * filter_buff[2]
      |                             + stripe_cache[      7] * filter_buff[5]
      |                             + stripe_cache[      8] * filter_buff[8];
      |                // End filter
      |                out_row_buff[out_lane++] = out;
      |                fid += filter_size;
      |
      |                for (int rc = 0; rc < 6; rc++) {
      |                    stripe_cache[rc] = stripe_cache[rc + 3];
      |                }
      |                stripe_cache[6] = channel_buff[fid + 6];
      |                stripe_cache[7] = channel_buff[fid + 7];
      |                stripe_cache[8] = channel_buff[fid + 8];
      |
      |                // Row buffer full or last lane?
      |                if (out_lane == row_buff_size || (cw == width - 1)) {
      |                    barrier(CLK_LOCAL_MEM_FENCE);
      |                    if (lx < out_lane) {
      |                        // lx = channels 2 or 8, ly = outputs 32
      |                        // repurpose the lx threads over columns now
      |                        if (chan_buff_size == 8) {
      |                            float val;
      |                            val  = row_buff[(ly * chan_buff_size + 0) * row_buff_size + lx];
      |                            val += row_buff[(ly * chan_buff_size + 1) * row_buff_size + lx];
      |                            val += row_buff[(ly * chan_buff_size + 2) * row_buff_size + lx];
      |                            val += row_buff[(ly * chan_buff_size + 3) * row_buff_size + lx];
      |                            val += row_buff[(ly * chan_buff_size + 4) * row_buff_size + lx];
      |                            val += row_buff[(ly * chan_buff_size + 5) * row_buff_size + lx];
      |                            val += row_buff[(ly * chan_buff_size + 6) * row_buff_size + lx];
      |                            val += row_buff[(ly * chan_buff_size + 7) * row_buff_size + lx];
      |                            vstore_net_t(val, (((c >> chan_shift) * height + row) * width + out_cw + lx) * outputs + o, merge);
      |                        } else if (chan_buff_size == 2) {
      |                            float val;
      |                            val  = row_buff[(ly * chan_buff_size + 0) * row_buff_size + lx];
      |                            val += row_buff[(ly * chan_buff_size + 1) * row_buff_size + lx];
      |                            vstore_net_t(val, (((c >> chan_shift) * height + row) * width + out_cw + lx) * outputs + o, merge);
      |                        }
      |                    }
      |                    out_cw  += row_buff_size;
      |                    out_lane = 0;
      |                }
      |            }
      |        }
      |    }
      |)""".stripMargin

    val sourceCode_utility: String = """(
      |    __kernel void merge(
      |                        __global const net_t * in,
      |                        __global net_t * out,
      |                        __constant const net_t * biases,
      |                        __private const int channels) {
      |
      |        // cl::NDRange global(outputs, 19*19);
      |        const int gx = get_global_id(0);
      |        const int gy = get_global_id(1);
      |
      |        const int output = gx;
      |        const int b = gy;
      |        const int outputs = get_global_size(0);
      |
      |        const int width = 19;
      |        const int height = 19;
      |        const int boardsize = width * height;
      |
      |        const int o = output;
      |        const float bias = vload_net_t(o, biases);
      |
      |        float sum = bias;
      |        for (int c = 0; c < channels; c++) {
      |            sum += vload_net_t((c * boardsize + b) * outputs + o, in);
      |        }
      |        vstore_net_t(sum, o * boardsize + b, out);
      |    }
      |
      |    __kernel void batchnorm(
      |                        __global const net_t * in,
      |                        __global net_t * out,
      |                        __global const net_t * residual,
      |                        __constant const net_t * means,
      |                        __constant const net_t * variances) {
      |
      |        // cl::NDRange global(outputs, 19*19);
      |        const int gx = get_global_id(0);
      |        const int gy = get_global_id(1);
      |
      |        const int output = gx;
      |        const int outputs      = get_global_size(0);
      |        const int channel_size = get_global_size(1);
      |
      |        const unsigned int o = output;
      |        const unsigned int b = gy;
      |
      |        const float epsilon = 1e-5;
      |
      |        const float mean = vload_net_t(o, means);
      |        const float variance = epsilon + vload_net_t(o, variances);
      |        const float scale_stddiv = 1.0f / sqrt(variance);
      |
      |        // BN
      |        float sum = scale_stddiv * (vload_net_t(o * channel_size + b, in) - mean);
      |        // Residual Eltwise
      |        if (residual) {
      |            sum += vload_net_t(o * channel_size + b, residual);
      |        }
      |        // ReLU
      |        vstore_net_t(sum > 0 ? sum : 0.0f, o * channel_size + b, out);
      |    }
      |)""".stripMargin


    /**
    * Returns the value of the platform info parameter with the given name
    * @param platform  The platform
    * @param paramName The parameter name
    * @return The value
    */
  private def getPlatformInfo(platform: cl_platform_id, paramName: Int): String = {
    val size = new Array[Long](1)
    clGetPlatformInfo(platform, paramName, 0, null, size)
    val buffer = new Array[Byte](size(0).toInt)
    clGetPlatformInfo(platform, paramName, buffer.length, Pointer.to(buffer), null)
    new String(buffer, 0, buffer.length - 1)
  }

  /**
    * Returns the value of the device info parameter with the given name
    * @param device    The device
    * @param paramName The parameter name
    * @return The value
    */
  private def getDeviceInfo(device: cl_device_id, paramName: Int) = {
    val size = new Array[Long](1)
    clGetDeviceInfo(device, paramName, 0, null, size)
    val buffer = new Array[Byte](size(0).toInt)
    clGetDeviceInfo(device, paramName, buffer.length, Pointer.to(buffer), null)
    new String(buffer, 0, buffer.length - 1)
  }

  import org.jocl.Sizeof
  import org.jocl.cl_device_id
  import java.nio.ByteBuffer
  import java.nio.ByteOrder

  /**
    * Returns the value of the device info parameter with the given name
    * @param device    The device
    * @param paramName The parameter name
    * @return The value
    */
  private def getDeviceSize(device: cl_device_id, paramName: Int) = getDeviceSizes(device, paramName, 1)(0)

  /**
    * The size of the returned data has to depend on the size of a size_t
    * (either 4 or 8 bytes depending on 32 or 64 bit OS)
    * @param device    The device
    * @param paramName The parameter name
    * @param numValues The number of values
    * @return values of the device info parameter with the given name
    */
  def getDeviceSizes(device: cl_device_id, paramName: Int, numValues: Int): Array[Long] = {
    val buffer = ByteBuffer.allocate(numValues * Sizeof.size_t).order(ByteOrder.nativeOrder)
    clGetDeviceInfo(device, paramName, Sizeof.size_t * numValues, Pointer.to(buffer), null)
    val values = new Array[Long](numValues)
    if (Sizeof.size_t == 4) {
      for (i <- 0 until numValues) {
        values(i) = buffer.getInt(i * Sizeof.size_t)
      }
    }
    else {
      for (i <- 0 until numValues) {
        values(i) = buffer.getLong(i * Sizeof.size_t)
      }
    }
    values
  }

  /**
    * Returns the value of the workgroup info parameter with the given name.
    * This method is probably not correct, but not really used for anything importatnt either.
    * @param kernel  The kernel
    * @param device  The device
    * @param paramName The parameter name
    * @return The value
    */
  private def getWorkGroupInfo(kernel: cl_kernel, device: cl_device_id, paramName: Int): Long = {

    val size = new Array[Long](1)
    val buffer = new Array[Byte](Sizeof.cl_ulong)
    val r = clGetKernelWorkGroupInfo(kernel, device, paramName, Sizeof.cl_ulong, Pointer.to(buffer), null)
    //val s = new String(buffer, 0, buffer.length - 1)
    Math.abs(r)
  }
}


/**
  * Facade into OpenCL code
  */
class OpenCL {

  private var program: cl_program  = _
  private val openclThreadData: ThreadLocal[ThreadData] = new ThreadLocal()
  openclThreadData.set(new ThreadData())

  var waveFrontSize: Long = 0
  var maxWorkGroupSize: Long = 0
  var maxWorkGroupDims: Array[Long] = _
  var initOk = false
  var context: cl_context = _
  var bestDevice: cl_device_id = _

  def initialize(): Unit = {
    // The platform and device type that will be used
    val platformIndex = 0
    val deviceType = CL_DEVICE_TYPE_ALL
    JCudaDriver.setExceptionsEnabled(true)

    cuInit(0)

    // Obtain the number of devices
    val deviceCountArray = Array(0)
    cuDeviceGetCount(deviceCountArray)
    val deviceCount = deviceCountArray(0)
    println("Found " + deviceCount + " devices")

    // Obtain the number of platforms
    val numPlatformsArray = new Array[Int](1)
    clGetPlatformIDs(0, null, numPlatformsArray)
    val numPlatforms = numPlatformsArray(0)
    //var platforms: Seq[cl_platform] = cl_platform.get()

    // Obtain a platform ID
    val platforms = new Array[cl_platform_id](numPlatforms)
    clGetPlatformIDs(platforms.length, platforms, null)
    val platform: cl_platform_id = platforms(platformIndex)

    var bestVersion: Float = 0.0f
    var bestPlatform: cl_platform_id = null
    var bestScore: Int = 0
    var foundDevice: Boolean = false

    myPrint(f"Detected $numPlatforms%d OpenCL platforms") // should only be one

    val platVersion = getPlatformInfo(platform, CL_PLATFORM_VERSION)
    val prefixLen = "OpenCL ".length
    val openClVersion = platVersion.substring(prefixLen, prefixLen + 4)
    val platProfile = getPlatformInfo(platform, CL_PLATFORM_PROFILE)
    val platName = getPlatformInfo(platform, CL_PLATFORM_NAME)
    val platVendor = getPlatformInfo(platform, CL_PLATFORM_VENDOR)
    myPrint(f"Platform version: $platVersion%s")
    myPrint(f"Platform profile: $platProfile%s")
    myPrint(f"Platform name:    $platName%s")
    myPrint(f"Platform vendor:  $platVendor%s")

    val openclVersion: Float = openClVersion.toFloat

    // Initialize the context properties
    val contextProperties = new cl_context_properties
    contextProperties.addProperty(CL_CONTEXT_PLATFORM, platform)

    // var devices: Seq[cl_device_id] = Seq()
    // Obtain the number of devices for the platform
    val numDevicesArray = new Array[Int](1)
    clGetDeviceIDs(platform, deviceType, 0, null, numDevicesArray)
    val numDevices = numDevicesArray(0)

    // Obtain a device IDs
    val devices = new Array[cl_device_id](numDevices)
    clGetDeviceIDs(platform, deviceType, numDevices, devices, null)
    for (id <- 0 until numDevices) {
      val device = devices(id)
      val deviceName =  getDeviceInfo(device, CL_DEVICE_NAME)
      val deviceType = getDeviceSize(device, CL_DEVICE_TYPE) //getDeviceInfo(device, CL_DEVICE_TYPE)
      val deviceVendor = getDeviceInfo(device, CL_DEVICE_VENDOR)
      val deviceDriver = getDeviceInfo(device, CL_DRIVER_VERSION)
      val deviceSpeed = getDeviceSize(device, CL_DEVICE_MAX_CLOCK_FREQUENCY) //getDeviceInfo(device, CL_DEVICE_MAX_CLOCK_FREQUENCY)
      val deviceCores = getDeviceSize(device, CL_DEVICE_MAX_COMPUTE_UNITS)
      myPrint(f"Device ID:     $id%d")
      myPrint(f"Device name:   $deviceName%s")
      myPrint(f"Device type:   $deviceType%s")
      myPrint(f"Device vendor: $deviceVendor%s")
      myPrint(f"Device driver: $deviceDriver%s")
      myPrint(f"Device speed:  $deviceSpeed%s MHz") // was %u
      myPrint(f"Device cores:  $deviceCores%s CU") // was %u

      // assign score, try to find best device (there is probably only one)
      var thisScore: Int = 0
      val thisVendor = deviceVendor
      if (thisVendor.contains("advanced micro devices") || thisVendor.contains("amd") || thisVendor.contains("nvidia"))
        thisScore += 1000
      else if (thisVendor.contains("intel"))
        thisScore += 500
      else if (deviceType == CL_DEVICE_TYPE_GPU)
        thisScore += 100
      thisScore += (openclVersion * 10).toInt

      myPrint(f"Device score:  $thisScore%d\n")
      val preferred = Config.cfg_preffered_gpus.contains(id)

      if ((thisScore > bestScore) || preferred) {
        bestVersion = openclVersion
        bestPlatform = platform
        bestDevice = device
        bestScore = if (preferred) Int.MaxValue else thisScore
        foundDevice = true
      }
    }

    if (!foundDevice) {
      throw new IllegalStateException("No suitable OpenCL device found.")
    }

    // cl::Platform::setDefault(bestPlatform)  how to set the platform? See contextProperties above
    val bestDeviceName = getDeviceInfo(bestDevice, CL_DEVICE_NAME).trim
    myPrint(f"Selected device: $bestDeviceName%s\n")
    myPrint(f"with OpenCL $bestVersion%2.1f capability\n")

    context = clCreateContext(contextProperties, 1, Array(bestDevice), null, null, null)

    //cl::Context::setDefault(context)?
    //cl::Device::setDefault(best_device)?

    // Read source file
    //std::ifstream sourceFile("convolve_kernel.cl", std::ifstream::in);
    //std::string sourceCode(std::istreambuf_iterator<char>(sourceFile),
    //                       (std::istreambuf_iterator<char>()));

    // Make program of the source code in the context
    // Create and build the program and the kernel
    val src =  Array[String](sourceCode_config, sourceCode_convolve1, sourceCode_convolve3, sourceCode_utility)
    val program = clCreateProgramWithSource(context, 1,src, null, null)

    // Build program for these specific devices
    var args = "-cl-mad-enable -cl-fast-relaxed-math -cl-no-signed-zeros -cl-denorms-are-zero"
    if (Config.USE_HALF)
      args += " -DUSE_HALF"

    clBuildProgram(program, 0, null, args, null, null) // program.build(args.c_str())
    val kernel = clCreateKernel(program, "sampleKernel", null)

    ensureThreadInitialized()

    waveFrontSize = getWorkGroupInfo(openclThreadData.get().convolve3Kernel, bestDevice,
      CL_KERNEL_PREFERRED_WORK_GROUP_SIZE_MULTIPLE)
    //  openclThreadData.get().convolve3Kernel.getWorkGroupInfo<CL_KERNEL_PREFERRED_WORK_GROUP_SIZE_MULTIPLE>(bestDevice)
    myPrint(f"Wavefront/Warp size: $waveFrontSize%d\n")

    maxWorkGroupSize = getDeviceSize(bestDevice, CL_DEVICE_MAX_WORK_GROUP_SIZE)
    val gdims = getDeviceSizes(bestDevice, CL_DEVICE_MAX_WORK_ITEM_SIZES, 3)
    println("workGrouDims = " + gdims)
    maxWorkGroupDims = gdims

    myPrint(f"Max workgroup size: $maxWorkGroupSize%d\n")
    myPrint(f"Max workgroup dimensions: ")
    for (d <- maxWorkGroupDims) {
      myPrint(f"$d%d ")
    }
    myPrint("\n")
    initOk = true
  }

  def ensureThreadInitialized(): Unit = {
    val threadData = openclThreadData.get()
    if (!threadData.isInitialized) {
      // Make kernels
      threadData.convolve1Kernel = clCreateKernel(program, "convolve1", null)
      threadData.convolve3Kernel = clCreateKernel(program, "convolve3", null)
      threadData.mergeKernel = clCreateKernel(program, "merge", null)
      threadData.batchNormKernel = clCreateKernel(program, "batchnorm", null)
      threadData.commandQueue = clCreateCommandQueueWithProperties(context, bestDevice, null, null)
      threadData.isInitialized = true
    }
  }
}
