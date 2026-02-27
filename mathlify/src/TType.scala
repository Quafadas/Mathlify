package mathlify

sealed trait TType
case object CONST         extends TType
case object UNARY         extends TType
case object BINARY        extends TType
case object INFIX         extends TType
case object LEFTBRACKET   extends TType
case object RIGHTBRACKET  extends TType
case object SPACE         extends TType
case object UNDEROVER     extends TType
case object DEFINITION    extends TType
case object LEFTRIGHT     extends TType
case object TEXTTYPE      extends TType
case object UNARYUNDEROVER extends TType