package PispLang.BuildIns

object Vars {
  sealed trait BuildInVar {
    def getName: String
  }

  case object Input extends BuildInVar {
    override def getName: String = "input"
  }

}
