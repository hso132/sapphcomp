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

