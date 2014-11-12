package option

import scalaz.std.option._

object Examples {

  def normal = {
    for {
      x <- Some(1)
      y <- Some(2)
      z <- Some(3)
    }
  }

}
