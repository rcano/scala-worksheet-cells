package worksheetcells

private[worksheetcells] object Aux {
  
  def toHex(bytes: Array[Byte]): String = String.format("%0" + (bytes.length << 1) + "X", new java.math.BigInteger(1, bytes))
}
