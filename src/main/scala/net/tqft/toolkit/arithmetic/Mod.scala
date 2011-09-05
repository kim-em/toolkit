package net.tqft.toolkit.arithmetic

object Mod {
	implicit def moddable(x: Int) = new Moddable(x)
	class Moddable(x: Int) {
	  def mod(k: Int) = {
	    require(k != 0)
	    if(x < 0) {
	      -((-x-1) % k) + k - 1
	    } else {
	      x % k
	    }
	  }
	}
}