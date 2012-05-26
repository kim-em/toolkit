package net.tqft.toolkit.arithmetic

object Mod {
	implicit def moddable(x: Int) = new ModdableInt(x)
	class ModdableInt(x: Int) {
	  def mod(k: Int) = {
	    require(k != 0)
	    if(x < 0) {
	      -((-x-1) % k) + k - 1
	    } else {
	      x % k
	    }
	  }
	}
	implicit def moddable(x: Long) = new ModdableLong(x)
	class ModdableLong(x: Long) {
	  def mod(k: Long) = {
	    require(k != 0)
	    if(x < 0) {
	      -((-x-1) % k) + k - 1
	    } else {
	      x % k
	    }
	  }
	}
}