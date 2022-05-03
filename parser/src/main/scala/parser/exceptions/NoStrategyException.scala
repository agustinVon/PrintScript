package parser.exceptions

case class NoStrategyException() extends Exception {
    @Override override def getMessage: String = "No strat"
}
