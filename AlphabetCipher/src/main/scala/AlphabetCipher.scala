
object AlphabetCipher{

  val lookup = Map("a"->1,"b"->2,"c"->3,"d"->4,"e"->5,"f"->6,"g"->7,"h"->8,"i"->9,"j"->10,"k"->11,"l"->12,"m"->13,"n"->14,
    "o"->15,"p"->16,"q"->17,"r"->18,"s"->19,"t"->20,"u"->21,"v"->22,"w"->23,"x"->24,"y"->25,"z"->26)
  val reverseLookup = lookup.map(_.swap)

  def encode(keyword: String, message: String): String = {
    val encoded = keyMap(keyword,message).map(x => if (x._1 + x._2 - 1 > 26) x._1 + x._2 - 27 else x._1 + x._2 - 1)
    encoded.map(reverseLookup).mkString
  }

  def keyMap(keyword: String, message: String): IndexedSeq[(Int,Int)] = {
    val expandedKey = expandKey(keyword,message.length)
    val messageNum = message.map(x => lookup(x.toString))
    val keyNum = expandedKey.map(x => lookup(x.toString))
    keyNum.zip(messageNum)
  }

  def expandKey(key: String, length: Int): String = {
    val timesToRepeat = length / key.length
    val remainder = length % key.length
    key * timesToRepeat + key.substring(0, remainder)
  }

  def decode(keyword: String, message: String): String = {
    val encoded = keyMap(keyword,message).map(x => if (x._2 - x._1 + 1 < 0) x._2 - x._1 + 27 else x._2 - x._1 + 1)
    encoded.map(reverseLookup).mkString
  }

  def decipher(cipher: String, message: String): String = {
    val encoded = keyMap(cipher,message).map(x => if (x._1 - x._2 + 1 <0) x._1 - x._2 + 27 else x._1 - x._2 + 1)
    val keyLength = encoded.map(reverseLookup).toSet.mkString.length +1
    encoded.map(reverseLookup).mkString.take(keyLength)
  }
}
