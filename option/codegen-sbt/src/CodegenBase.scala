package nonscala

import Base._

object CodegenBase {

  def notSupported(s: String) = throw(new MyError("not suppored: "+s))

  type Label = String

  var symNum = 0

  // 新しい変数(シンボル)を作る
  def freshVar(): Var = {
    val x = "_"+symNum.toInt
    symNum = symNum + 1
    x
  }

  // 新しいラベルを作る
  def freshLabel(s: String): Label = {
    val l = s+symNum.toInt
    symNum = symNum + 1
    l
  }
}
