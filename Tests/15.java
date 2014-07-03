/* Tree234.java */

package dict;

/**
 *  A Tree234 implements an ordered integer dictionary ADT using a 2-3-4 tree.
 *  Only int keys are stored; no object is associated with each key.  Duplicate
 *  keys are not stored in the tree.
 *
 *  @author Jonathan Shewchuk
 **/
public class Tree234 extends IntDictionary {

  /**
   *  You may add fields if you wish, but don't change anything that
   *  would prevent toString() or find() from working correctly.
   *
   *  (inherited)  size is the number of keys in the dictionary.
   *  root is the root of the 2-3-4 tree.
   **/
  Tree234Node root;

  /**
   *  Tree234() constructs an empty 2-3-4 tree.
   *
   *  You may change this constructor, but you may not change the fact that
   *  an empty Tree234 contains no nodes.
   */
  public Tree234() {
    root = null;
    size = 0;
  }

  /**
   *  toString() prints this Tree234 as a String.  Each node is printed
   *  in the form such as (for a 3-key node)
   *
   *      (child1)key1(child2)key2(child3)key3(child4)
   *
   *  where each child is a recursive call to toString, and null children
   *  are printed as a space with no parentheses.  Here's an example.
   *      ((1)7(11 16)22(23)28(37 49))50((60)84(86 95 100))
   *
   *  DO NOT CHANGE THIS METHOD.  The test code depends on it.
   *
   *  @return a String representation of the 2-3-4 tree.
   **/
  public String toString() {
    if (root == null) {
      return "";
    } else {
      /* Most of the work is done by Tree234Node.toString(). */
      return root.toString();
    }
  }

  /**
   *  printTree() prints this Tree234 as a tree, albeit sideways.
   *
   *  You're welcome to change this method if you like.  It won't be tested.
   **/
  public void printTree() {
    if (root != null) {
      /* Most of the work is done by Tree234Node.printSubtree(). */
      root.printSubtree(0);
    }
  }

  /**
   *  find() prints true if "key" is in this 2-3-4 tree; false otherwise.
   *
   *  @param key is the key sought.
   *  @return true if "key" is in the tree; false otherwise.
   **/
  public boolean find(int key) {
    Tree234Node node = root;
    while (node != null) {
      if (key < node.key1) {
        node = node.child1;
      } else if (key == node.key1) {
        return true;
      } else if ((node.keys == 1) || (key < node.key2)) {
        node = node.child2;
      } else if (key == node.key2) {
        return true;
      } else if ((node.keys == 2) || (key < node.key3)) {
        node = node.child3;
      } else if (key == node.key3) {
        return true;
      } else {
        node = node.child4;
      }
    }
    return false;
  }

  /**
   *  insert() inserts the key "key" into this 2-3-4 tree.  If "key" is
   *  already present, a duplicate copy is NOT inserted.
   *
   *  @param key is the key sought.
   **/
  public void insert(int key) {
    // Fill in your solution here.
    if (find(key)) {
      return;
    }
    Tree234Node trackerNode = root;
    //System.out.println(trackerNode == null);
    while (trackerNode != null) {
      if (trackerNode.keys == 3) {
        //System.out.println("lol");
        Tree234Node newParent;
        //Tree234Node newChild1 = new Tree234Node(newParent, trackerNode.key1);
        //Tree234Node newChild2 = new Tree234Node(newParent, trackerNode.key3);
        //if the root node has 3 keys
        if (trackerNode.parent == null) { 
          newParent = new Tree234Node(null, trackerNode.key2); //create a new root
          this.root = newParent;
          Tree234Node newChild1 = new Tree234Node(newParent, trackerNode.key1);
          //System.out.println("newChild1's parent is: "+newChild1.parent);
          Tree234Node newChild2 = new Tree234Node(newParent, trackerNode.key3);
          newParent.child1 = newChild1;
          newParent.child2 = newChild2;
          if (trackerNode.child1 != null) {
            trackerNode.child1.parent = newChild1;
            newChild1.child1 = trackerNode.child1;
          }
          if (trackerNode.child2 != null) {
            trackerNode.child2.parent = newChild1;
            newChild1.child2 = trackerNode.child2;
          }
          if (trackerNode.child3 != null) {
            trackerNode.child3.parent = newChild2;
            newChild2.child1 = trackerNode.child3;
          }
          if (trackerNode.child4 != null) {
            trackerNode.child4.parent = newChild2;
            newChild2.child2 = trackerNode.child4;
          }
          
          //System.out.println("here comes the new parent " +newParent);
          if (key > newParent.key1) {
            //System.out.println("im here");
            trackerNode = newChild2;
          }else {
            trackerNode = newChild1;
            //System.out.println(trackerNode.parent);
          }
        //if it's the internal node that has 3 keys
        }else {
          //System.out.println("trackerNode was: "+trackerNode.child1); //--null

          //int largestKey = trackerNode.key3;
          Tree234Node oldParent = trackerNode.parent;
          //System.out.println("junyu "+oldParent + "," + trackerNode);

          //System.out.println("trackerNode's parent: "+trackerNode.parent);

          newParent = new Tree234Node(oldParent.parent, 0);
          Tree234Node newChild1 = new Tree234Node(newParent, trackerNode.key1);
          Tree234Node newChild2 = new Tree234Node(newParent, trackerNode.key3);

          //newParent.child1 = newChild1

          
          //System.out.println("What's wrong");
          
          if (trackerNode.key3 > oldParent.key1) {
            //System.out.println("wow");
            if (trackerNode.key3 > oldParent.key2 && oldParent.key2 != 0) {
              //if the trackerNode is the thrid child
              newParent.key1 = oldParent.key1;
              newParent.key2 = oldParent.key2;
              newParent.key3 = trackerNode.key2;

              if (newParent.key3 == 0) {
                newParent.keys++;
              }else {
                newParent.keys += 2;
              }

              oldParent.child1.parent = newParent;
              newParent.child1 = oldParent.child1;
              oldParent.child2.parent = newParent;
              newParent.child2 = oldParent.child2;
              newParent.child3 = newChild1;
              newParent.child4 = newChild2;
            }else {
              //if trackerNode is the second child 
              newParent.key1 = oldParent.key1;
              newParent.key2 = trackerNode.key2;
              newParent.key3 = oldParent.key2;

              if (newParent.key3 == 0) {
                if (newParent.key2 != 0) {
                  newParent.keys++;
                }
              }else if (newParent.key3 != 0 && newParent.key2 != 0) {
                newParent.keys += 2;
              }

              //System.out.println(newParent.key2);

              oldParent.child1.parent = newParent;
              newParent.child1 = oldParent.child1;
              newParent.child2 = newChild1;
              newParent.child3 = newChild2;
              if (oldParent.keys == 2) {
                oldParent.child3.parent = newParent;
                newParent.child4 = oldParent.child3;
              }  
            }
          //if trackerNode is the 1st child 
          }else {
            newParent.key1 = trackerNode.key2;
            newParent.key2 = oldParent.key1;
            newParent.key3 = oldParent.key2;
            if (newParent.key3 == 0) {
              if (newParent.key2 != 0) {
                newParent.keys++;
              }
            }else if (newParent.key3 != 0 && newParent.key2 != 0) {
              newParent.keys += 2;
            }
            //set up the relation ship between the newParent and its children
            newParent.child1 = newChild1;
            newParent.child2 = newChild2;
            if (oldParent.child2 != null) {
              oldParent.child2.parent = newParent;
            newParent.child3 = oldParent.child2;
            }
            if (oldParent.child3 != null) {
              oldParent.child3.parent = newParent;
              newParent.child4 = oldParent.child3;
            }  
          }
          //System.out.println("new parent is: "+newParent);
          //if (oldParent.parent != null) {
            //System.out.println(oldParent == oldParent.parent.child2);
          //}
          //set up the paretn-child relationship between the newParent and the oldParent's parent
          if (oldParent.parent == null) {
            this.root = newParent;
          }else if (oldParent == oldParent.parent.child1) {
            oldParent.parent.child1 = newParent;
          }else if (oldParent == oldParent.parent.child2) {
            oldParent.parent.child2 = newParent;
            //System.out.println("old parent's paretn "+oldParent.parent.child2);
          }else {
            oldParent.parent.child3 = newParent;
          }
          //System.out.println("wohoalaa "+newParent);
          //if trackerNode is not the leaf, set up the parent
          //System.out.println("hooso" + trackerNode);
          if (trackerNode.child1 != null) {
            //System.out.println("trackerNode is: "+trackerNode);
            newChild1.child1 = trackerNode.child1;
            trackerNode.child1.parent = newChild1;

            newChild1.child2 = trackerNode.child2;
            trackerNode.child2.parent = newChild1;

            newChild2.child1 = trackerNode.child3;
            trackerNode.child3.parent = newChild2;

            newChild2.child2 = trackerNode.child4;
            trackerNode.child4.parent = newChild2;

            //and update trackerNode
            if (key > trackerNode.key2) {
              trackerNode = newChild2;
            }else if (key < trackerNode.key2) {
              trackerNode = newChild1;
            }//(if key == trackerNode.key2, do nothing because we don't insert duplicate keys)
          //we add things according to the key, if trackerNode is the leaf
          }else {
            //System.out.println("wolala "+newParent);
            if (key > newParent.key3 && newParent.key3 != 0) {
              if (key > newParent.child4.key2 && newParent.child4.key2 != 0) {
                newParent.child4.key3 = key;
              }else if (key > newParent.child4.key1) {
                newParent.child4.key2 = key;
              }else {
                newParent.child4.key2 = newParent.child4.key1;
                newParent.child4.key1 = key;
              }
              newParent.child4.keys++;
              size++;
            }else if (key > newParent.key2 && newParent.key2 != 0) {
              if (key > newParent.child3.key2 && newParent.child3.key2 != 0) {
                newParent.child3.key3 = key;
              }else if (key > newParent.child3.key1) {
                newParent.child3.key2 = key;
              }else {
                newParent.child3.key2 = newParent.child3.key1;
                newParent.child3.key1 = key;
              }
              newParent.child3.keys++;
              size++;
              //System.out.println(newParent);
            }else if (key > newParent.key1) {
              if (key > newParent.child2.key2 && newParent.child2.key2 != 0) {
                newParent.child2.key3 = key;
              }else if (key > newParent.child2.key1) {
                newParent.child2.key2 = key;
              }else {
                newParent.child2.key2 = newParent.child2.key1;
                newParent.child2.key1 = key;
              }
              newParent.child2.keys++;
              size++;
            }else {
              if (key > newParent.child1.key2 && newParent.child1.key2 != 0) {
                newParent.child1.key3 = key;
              }else if (key > newParent.child1.key1) {
                newParent.child1.key2 = key;
              }else {
                newParent.child1.key2 = newParent.child1.key1;
                newParent.child1.key1 = key;
              }
              newParent.child1.keys++;
              size++;
            }
            return;
          }  
        }
      //if trackerNode has less than 3 keys
      }else if (trackerNode.keys == 2) {
        //if trackerNode is the leaf, we add things
        if (trackerNode.child1 == null) {
          //System.out.println("hahahaha");
          if (key > trackerNode.key2) {
            trackerNode.key3 = key;
            trackerNode.keys++;
            size++;
          }else if (key > trackerNode.key1) {
            trackerNode.key3 = trackerNode.key2;
            trackerNode.key2 = key;
            trackerNode.keys++;
            size++;
          }else {
            trackerNode.key3 = trackerNode.key2;
            trackerNode.key2 = trackerNode.key1;
            trackerNode.key1 = key;
            trackerNode.keys++;
            size++;
          }
          return;
        }else {
          if (key > trackerNode.key2) {
            trackerNode = trackerNode.child3;
          }else if (key > trackerNode.key1) {
            trackerNode = trackerNode.child2;
          }else {
            trackerNode = trackerNode.child1;
          }
        }   
      //if trackerNode only has one key  
      }else if (trackerNode.keys == 1) {

        //System.out.println("hehe");
        if (trackerNode.child1 == null) {
          if (key > trackerNode.key1) {
            trackerNode.key2 = key;
            trackerNode.keys++;
            size++;
            //System.out.println("hello");
            return;
          }else {
            trackerNode.key2 = trackerNode.key1;
            //System.out.println("haha"+trackerNode.key2);
            trackerNode.key1 = key;
            trackerNode.keys++;
            size++;
            //System.out.println("hehe"+trackerNode.key1+","+trackerNode.key2);
            //System.out.println("trackerNode is: "+trackerNode);
            //this.root = trackerNode;
            return;
          }
        }else {
          //System.out.println("what "+trackerNode.parent);
          if (key > trackerNode.key1) {
            trackerNode = trackerNode.child2;
          }else {
            //System.out.println("bad "+trackerNode.child1.parent);
            trackerNode = trackerNode.child1;
            //System.out.println("good " +trackerNode);
          }
        }
      }
    }
    if (trackerNode == null) {
      //System.out.println("hey");
      trackerNode = new Tree234Node(null, key);
      size++;
      this.root = trackerNode;
    } 
  }


  /**
   *  testHelper() prints the String representation of this tree, then
   *  compares it with the expected String, and prints an error message if
   *  the two are not equal.
   *
   *  @param correctString is what the tree should look like.
   **/
  public void testHelper(String correctString) {
    String treeString = toString();
    System.out.println(treeString);
    if (!treeString.equals(correctString)) {
      System.out.println("ERROR:  Should be " + correctString);
    }
  }

  /**
   *  main() is a bunch of test code.  Feel free to add test code of your own;
   *  this code won't be tested or graded.
   **/
  public static void main(String[] args) {
    Tree234 t = new Tree234();

    System.out.println("\nInserting 84.");
    t.insert(84);
    t.testHelper("84");
    System.out.println("size is: "+t.size);

    System.out.println("\nInserting 7.");
    t.insert(7);
    //System.out.println(t);
    t.testHelper("7 84");
    System.out.println("size is: "+t.size);

    System.out.println("\nInserting 22.");
    t.insert(22);
    t.testHelper("7 22 84");
    System.out.println("size is: "+t.size);

    System.out.println("\nInserting 95.");
    t.insert(95);
    t.testHelper("(7)22(84 95)");
    System.out.println("size is: "+t.size);

    System.out.println("\nInserting 50.");
    t.insert(50);
    t.testHelper("(7)22(50 84 95)");
    System.out.println("size is: "+t.size);

    System.out.println("\nInserting 11.");
    t.insert(11);
    t.testHelper("(7 11)22(50 84 95)");
    System.out.println("size is: "+t.size);

    System.out.println("\nInserting 37.");
    t.insert(37);
    t.testHelper("(7 11)22(37 50)84(95)");
    System.out.println("size is: "+t.size);

    System.out.println("\nInserting 60.");
    t.insert(60);
    t.testHelper("(7 11)22(37 50 60)84(95)");
    System.out.println("size is: "+t.size);

    System.out.println("\nInserting 1.");
    t.insert(1);
    t.testHelper("(1 7 11)22(37 50 60)84(95)");
    System.out.println("size is: "+t.size);

    System.out.println("\nInserting 23.");
    t.insert(23);
    t.testHelper("(1 7 11)22(23 37)50(60)84(95)");
    System.out.println("size is: "+t.size);

    System.out.println("\nInserting 16.");
    t.insert(16);
    t.testHelper("((1)7(11 16)22(23 37))50((60)84(95))");
    System.out.println("size is: "+t.size);

    System.out.println("\nInserting 100.");
    t.insert(100);
    t.testHelper("((1)7(11 16)22(23 37))50((60)84(95 100))");
    System.out.println("size is: "+t.size);

    System.out.println("\nInserting 28.");
    t.insert(28);
    t.testHelper("((1)7(11 16)22(23 28 37))50((60)84(95 100))");
    System.out.println("size is: "+t.size);

    System.out.println("\nInserting 86.");
    t.insert(86);
    t.testHelper("((1)7(11 16)22(23 28 37))50((60)84(86 95 100))");
    System.out.println("size is: "+t.size);

    System.out.println("\nInserting 49.");
    t.insert(49);
    t.testHelper("((1)7(11 16)22(23)28(37 49))50((60)84(86 95 100))");
    System.out.println("size is: "+t.size);

    System.out.println("\nInserting 81.");
    t.insert(81);
    t.testHelper("((1)7(11 16)22(23)28(37 49))50((60 81)84(86 95 100))");
    System.out.println("size is: "+t.size);

    System.out.println("\nInserting 51.");
    t.insert(51);
    t.testHelper("((1)7(11 16)22(23)28(37 49))50((51 60 81)84(86 95 100))");
    System.out.println("size is: "+t.size);

    System.out.println("\nInserting 99.");
    t.insert(99);
    t.testHelper("((1)7(11 16)22(23)28(37 49))50((51 60 81)84(86)95(99 100))");
    System.out.println("size is: "+t.size);

    System.out.println("\nInserting 75.");
    t.insert(75);
    t.testHelper("((1)7(11 16)22(23)28(37 49))50((51)60(75 81)84(86)95" +
                 "(99 100))");
    System.out.println("size is: "+t.size);

    System.out.println("\nInserting 66.");
    t.insert(66);
    t.testHelper("((1)7(11 16)22(23)28(37 49))50((51)60(66 75 81))84((86)95" +
                 "(99 100))");
    System.out.println("size is: "+t.size);

    System.out.println("\nInserting 4.");
    t.insert(4);
    t.testHelper("((1 4)7(11 16))22((23)28(37 49))50((51)60(66 75 81))84" +
                 "((86)95(99 100))");
    System.out.println("size is: "+t.size);

    System.out.println("\nInserting 80.");
    t.insert(80);
    t.testHelper("(((1 4)7(11 16))22((23)28(37 49)))50(((51)60(66)75" +
                 "(80 81))84((86)95(99 100)))");
    System.out.println("size is: "+t.size);

    System.out.println("\nFinal tree:");
    t.printTree();
  }

}
