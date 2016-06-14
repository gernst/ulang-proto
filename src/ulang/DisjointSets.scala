package ulang

/**
 * Persistent union-find data structure,
 * losely based on:
 * Sylvain Conchon, Jean-Christophe Filliatre:
 * A Persistent Union-Find Data Structure
 */

class DisjointSets[A](var parent: Map[A, A]) {
  /**
   * Get representative of a in parent0
   *  and path-compressed map.
   */
  def find_aux(a: A, parent0: Map[A, A]): (A, Map[A, A]) = {
    val b = parent0.getOrElse(a, a)
    if (b == a) {
      (a, parent0)
    } else {
      val (r, parent1) = find_aux(b, parent0)
      (r, parent1 + (a -> r))
    }
  }

  /**
   * Get representative of a.
   *  Path compression is achieved as non-visible side-effect.
   */
  def find(a: A): A = {
    val (r, parent1) = find_aux(a, parent)
    parent = parent1
    r
  }

  /** Set representative of a1 to a2 (not commutative) */
  def union(a1: A, a2: A): DisjointSets[A] = {
    val r1 = find(a1)
    val r2 = find(a2)
    new DisjointSets[A](parent + (r1 -> r2))
  }

  override def toString = {
    val m = parent.keys map { a => (find(a), a) }
    val g = m.groupBy(_._1).toList
    val gs = g map {
      case (r, c) =>
        c.map(_._2).mkString("{ " + r + "*, ", ", ", " }")
    }
    gs.mkString("\n")
  }
}

object DisjointSets {
  def empty[A] = new DisjointSets[A](Map.empty)
}
