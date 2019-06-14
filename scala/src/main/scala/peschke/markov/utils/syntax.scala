package peschke.markov.utils

object syntax {
  implicit class MapOps[K,V](val kvMap: Map[K,V]) extends AnyVal {
    /**
      * I'm not sure why this doesn't exist as part of the standard library, I
      * nearly always end up recreating it.
      *
      * The value is looked up, and either modified with `f` or `default` is taken
      * instead, and this value is loaded back into `kvMap`
      *
      * @param key The map key
      * @param default Used if there is no value for `key`
      * @param f Applied to the value for `key`, if one exists.
      * @return
      */
    def setOrCompute(key: K, default: => V)(f: V => V): Map[K,V] =
      kvMap.updated(key, kvMap.get(key).fold(default)(f))
  }

  implicit class StringOps(val str: String) extends AnyVal {
    /**
      * Upcases only the first letter, leaving the rest untouched
      */
    def toTitleCase: String = {
      val first = str.take(1).toUpperCase
      val rest = str.drop(1)
      s"$first$rest"
    }
  }
}
