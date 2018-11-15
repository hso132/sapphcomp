object L {
  abstract class List
  case class Nil() extends List
  case class Cons(h: Int, t: List) extends List
 
  def isEmpty(l : List): Boolean = { l match {
    case Nil() => true
    case _ => false 
  }}

  def length(l: List): Int = { l match {
    case Nil() => 0
    case Cons(_, t) => 1 + length(t)
  }}

  def head(l: List): Int = {
    l match {
      case Cons(h, _) => h
      case Nil() => error("head(Nil)")
    }
  }

  def headOption(l: List): O.Option = {
    l match {
      case Cons(h, _) => O.Some(h)
      case Nil() => O.None()
    }
  }

  def reverse(l: List): List = {
    reverseAcc(l, Nil())
  }

  def reverseAcc(l: List, acc: List): List = {
    l match {
      case Nil() => acc
      case Cons(h, t) => reverseAcc(t, Cons(h, acc))
    }
  }

  def indexOf(l: List, i: Int): Int = {
    l match {
      case Nil() => -1
      case Cons(h, t) =>
        if (h == i) { 0 }
        else {
          val rec: Int = indexOf(t, i);
          if (0 <= rec) { rec + 1 }
          else { -1 }
        }
    }
  }

  def range(from: Int, to: Int): List = {
    if (to < from) { Nil() }
    else {
      Cons(from, range(from + 1, to))
    }
  }

  def sum(l: List): Int = { l match {
    case Nil() => 0
    case Cons(h, t) => h + sum(t)
  }}

  def concat(l1: List, l2: List): List = {
    l1 match {
      case Nil() => l2
      case Cons(h, t) => Cons(h, concat(t, l2))
    }
  }

  def contains(l: List, elem: Int): Boolean = { l match {
    case Nil() =>
      false
    case Cons(h, t) =>
      h == elem || contains(t, elem)
  }}

  abstract class LPair
  case class LP(l1: List, l2: List) extends LPair

  def merge(l1: List, l2: List): List = {
    l1 match {
      case Nil() => l2
      case Cons(h1, t1) =>
        l2 match {
          case Nil() => l1
          case Cons(h2, t2) =>
            if (h1 <= h2) {
              Cons(h1, merge(t1, l2))
            } else {
              Cons(h2, merge(l1, t2))
            }
        }
    }
  }

  def split(l: List): LPair = {
    l match {
      case Cons(h1, Cons(h2, t)) =>
        val rec: LPair = split(t);
        rec match {
          case LP(rec1, rec2) =>
            LP(Cons(h1, rec1), Cons(h2, rec2))
        }
      case _ =>
        LP(l, Nil())
    }
  }
  def mergeSort(l: List): List = {
    l match {
      case Nil() => l
      case Cons(h, Nil()) => l
      case l =>
        split(l) match {
          case LP(l1, l2) =>
            merge(mergeSort(l1), mergeSort(l2))
        }
    }
  }
  
  def toString(l: List): String = { l match {
    case Nil() => "List()"
    case more => "List(" ++ toString1(more) ++ ")"
  }}

  def toString1(l : List): String = { l match {
    case Cons(h, Nil()) => Std.intToString(h)
    case Cons(h, t) => Std.intToString(h) ++ ", " ++ toString1(t)
  }}

  def take(l: List, n: Int): List = {
    if (n <= 0) { Nil() }
    else { 
      l match {
        case Nil() => Nil()
        case Cons(h, t) =>
          Cons(h, take(t, n-1))
      }
    }
  }
    
}
object O {
  abstract class Option
  case class None() extends Option
  case class Some(v: Int) extends Option

  def isDefined(o: Option): Boolean = {
    o match {
      case None() => false
      case _ => true
    }
  }

  def get(o: Option): Int = {
    o match {
      case Some(i) => i
      case None() => error("get(None)")
    }
  }

  def getOrElse(o: Option, i: Int): Int = {
    o match {
      case None() => i
      case Some(oo) => oo
    }
  }

  def orElse(o1: Option, o2: Option): Option = {
    o1 match {
      case Some(_) => o1
      case None() => o2
    }
  }

  def toList(o: Option): L.List = {
    o match {
      case Some(i) => L.Cons(i, L.Nil())
      case None() => L.Nil()
    }
  }
}
/** This module contains basic functionality for Amy,
  * including stub implementations for some built-in functions
  * (implemented in WASM or JavaScript)
  */
object Std {
  def printInt(i: Int): Unit = {
    error("") // Stub implementation
  }
  def printString(s: String): Unit = {
    error("") // Stub implementation
  }
  def printBoolean(b: Boolean): Unit = {
    printString(booleanToString(b))
  }

  def readString(): String = {
    error("") // Stub implementation
  }

  def readInt(): Int = {
    error("") // Stub implementation
  }

  def intToString(i: Int): String = {
    if (i < 0) {
      "-" ++ intToString(-i)
    } else {
      val rem: Int = i % 10;
      val div: Int = i / 10;
      if (div == 0) { digitToString(rem) }
      else { intToString(div) ++ digitToString(rem) }
    }
  }
  def digitToString(i: Int): String = {
    error("") // Stub implementation
  }
  def booleanToString(b: Boolean): String = {
    if (b) { "true" } else { "false" }
  }
}
object TreeSet
{
    abstract class TreeNode
    case class Node(value: Int, left: TreeNode, right: TreeNode) extends TreeNode
    case class Leaf() extends TreeNode


    def insert(tree: TreeNode, i: Int): TreeNode =
    {
      tree match
      {
        case Leaf() => Node(i, Leaf(), Leaf())
        case Node(v,l,r) => 
          //i goes to the right
          if(v<i)
          {
            val rNode: TreeNode = insert(r,i);
            Node(v,l,rNode)
          }
          else
          {
            //i goes to the left
            if(i<v)
            {
              val lNode: TreeNode = insert(l,i);
              Node(v,lNode,r)
            }
            //i IS v
            else
            {
              tree
            }
          }
      }
    }

    def contains(tree: TreeNode, i: Int): Boolean = 
    {
      tree match
      {
        case Leaf() => false
        case Node(v,l,r) =>
          if(v==i)
          {
            true
          }
          else
          {
            if(v<i)
            {
            contains(r, i)
            }
            else
            {
              contains(l,i)
            }
          }
      }
    }

    def maxDepth(tree: TreeNode): Int =
    {
      tree match
      {
        case Leaf() => 0
        case Node(v,l,r) =>
          val left: Int = maxDepth(l);
          val right: Int = maxDepth(r);
          val max: Int = 
            if(right < left){left}
            else{right};
          max+1
      }
    }

    def size(tree: TreeNode): Int =
    {
      tree match
      {
        case Leaf() => 0
        case Node(_,l,r) => size(l)+size(r)+1
      }
    }

    def printContains(argTree: TreeNode, num: Int): Unit = 
    {
      Std.printString(Std.intToString(num) ++ " is in tree: " ++ Std.booleanToString(contains(argTree, num)))
    }
    


    val tree: TreeNode = insert(insert(insert(insert(insert(Leaf(), 5), 6), 3), 100),1);

    printContains(tree, 5);
    printContains(tree, 6);
    printContains(tree, 7);
    Std.printString("Max Depth: " ++ Std.intToString(maxDepth(tree)));
    Std.printString("Size: " ++ Std.intToString(size(tree)));
    ()
}

