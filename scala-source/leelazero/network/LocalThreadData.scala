package leelazero.network

/**
  * Thread meta data bound to a given thread
  */
class LocalThreadData extends ThreadLocal[ThreadData] {

  override def initialValue = new ThreadData()
}