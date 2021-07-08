package PispLang.BuildIns

object Functions {
  sealed trait BuildInFunction {
    def getName: String
  }

  case object Sum extends BuildInFunction {
    override def getName: String = "sum"
  }

  case object Prod extends BuildInFunction {
    override def getName: String = "prod"
  }

  case object Add extends BuildInFunction {
    override def getName: String = "add"
  }

  case object Mul extends BuildInFunction {
    override def getName: String = "mul"
  }

  case object Div extends BuildInFunction {
    override def getName: String = "div"
  }

  case object Sub extends BuildInFunction {
    override def getName: String = "sub"
  }

  case object Print extends BuildInFunction {
    override def getName: String = "print"
  }


}
