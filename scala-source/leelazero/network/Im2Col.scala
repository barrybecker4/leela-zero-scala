package leelazero.network

import java.nio.FloatBuffer

import leelazero.network.Network._

/**
  * This code is not used much if you have a powerful graphics card, but is for CPU only version.
  * Here are some references to help understand what its doing and why its written this way:
  * https://petewarden.com/2015/04/20/why-gemm-is-at-the-heart-of-deep-learning/
  * https://arxiv.org/pdf/1410.0759.pdf
  */
object Im2Col {

  def im2Col(filterSize: Int, channels: Int, input: FloatBuffer, output: Array[Float]): Unit = {
    val height = BOARD_SIZE
    val width = BOARD_SIZE
    val channelSize = height * width

    val pad = filterSize / 2
    val outputH: Int = height + 2 * pad - filterSize  + 1
    val outputW = width + 2 * pad - filterSize + 1

    var inputIdx = 0
    var outputIdx = 0
    //const net_t* data_im = input.data()
    //float* data_col = output.data()
    println("num channels = " + channels)

    for (channel <- (channels - 1) to 0 by -1) {
      for (kernelRow <- 0 until filterSize) {
        for (kernelCol <- 0 until filterSize) {
          var inputRow = kernelRow - pad
          for (outputRows <- outputH - 1 to 0 by -1) {
            if (inputRow < height) {
              var inputCol = kernelCol - pad
              for (outputCol <- outputW - 1 to 0 by -1) {
                if (inputCol < width) {
                  val idx = inputIdx + inputRow * width + inputCol
                  //println("input size = " + input.capacity() + " idx = " + idx + s" $inputIdx + $inputRow * $width + $inputCol")
                  output(outputIdx) = input.get(idx)
                  outputIdx += 1
                } else {
                  output(outputIdx) = 0
                  outputIdx += 1
                }
                inputCol += 1
              }
            } else {
              for (outputCols <- outputW to 0 by -1) {
                output(outputIdx) = 0
              }
            }
            inputRow += 1
          }
        }
      }
      inputIdx += channelSize
    }
  }
}

/*

template <unsigned long filter_size>
void im2col(const int channels,
            const std::vector<net_t>& input,
            std::vector<float>& output) {
    constexpr unsigned int height = 19;
    constexpr unsigned int width = 19;
    constexpr unsigned int channel_size = height * width;

    constexpr int pad = (filter_size / 2);
    constexpr unsigned int output_h = height + 2 * pad - filter_size  + 1;
    constexpr unsigned int output_w = width + 2 * pad - filter_size + 1;

    const net_t* data_im = input.data();
    float* data_col = output.data();

    for (int channel = channels; channel--; data_im += channel_size) {
        for (unsigned int kernel_row = 0; kernel_row < filter_size; kernel_row++) {
            for (unsigned int kernel_col = 0; kernel_col < filter_size; kernel_col++) {
                int input_row = -pad + kernel_row;
                for (int output_rows = output_h; output_rows; output_rows--) {
                    if ((unsigned)input_row < height) {
                        int input_col = -pad + kernel_col;
                        for (int output_col = output_w; output_col; output_col--) {
                            if ((unsigned)input_col < width) {
                                *(data_col++) =
                                    data_im[input_row * width + input_col];
                            } else {
                                *(data_col++) = 0;
                            }
                            input_col++;
                        }
                    } else {
                        for (int output_cols = output_w; output_cols; output_cols--) {
                            *(data_col++) = 0;
                        }
                    }
                    input_row++;
                }
            }
        }
    }
}

template <>
void im2col<1>(const int channels,
               const std::vector<net_t>& input,
               std::vector<float>& output) {
    constexpr unsigned int boardsize = 19;
    auto outSize = size_t{channels * boardsize * boardsize};
    assert(output.size() == outSize);
    std::copy(begin(input), begin(input) + outSize, begin(output));
}

#endif
 */