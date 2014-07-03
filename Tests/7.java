/* HashTableChained.java */
package dict;

import list.*;

/**
 *  HashTableChained implements a Dictionary as a hash table with chaining.
 *  All objects used as keys must have a valid hashCode() method, which is
 *  used to determine which bucket of the hash table an entry is stored in.
 *  Each object's hashCode() is presumed to return an int between
 *  Integer.MIN_VALUE and Integer.MAX_VALUE.  The HashTableChained class
 *  implements only the compression function, which maps the hash code to
 *  a bucket in the table's range.
 *
 *  DO NOT CHANGE ANY PROTOTYPES IN THIS FILE.
 **/

public class HashTableChained implements Dictionary {

  /**
   *  Place any data fields here.
   **/
  public int length;
  public DList[] bucketsArray;
  public int numOfEntries;
  public int numOfCollisions;




  /** 
   *  Construct a new empty hash table intended to hold roughly sizeEstimate
   *  entries.  (The precise number of buckets is up to you, but we recommend
   *  you use a prime number, and shoot for a load factor between 0.5 and 1.)
   **/

  public HashTableChained(int sizeEstimate) {
    // Your solution here.
    double loadFactor = 0.8;
    length = getPrimeNum((int) (sizeEstimate/loadFactor)); 
    bucketsArray = new DList[length];
    numOfEntries = sizeEstimate;
  }

  /** 
   *  Construct a new empty hash table with a default size.  Say, a prime in
   *  the neighborhood of 100.
   **/

  public HashTableChained() {
    // Your solution here.
    bucketsArray = new DList[101];
    length = 101;
    numOfEntries = 0;
  }

  //helper function, get prime number.
  private int getPrimeNum(int n) {
    boolean[] prime = new boolean[n+1];
    int i;
    for (i = 2; i <= n; i++) {
      prime[i] = true;
    }
    for (int divisor = 2; divisor*divisor <= n; divisor++) {
      if (prime[divisor]) {
        for (i = 2*divisor; i <= n; i += divisor) {
          prime[i] = false;
        }
      }
    }
    for(i = n; i>=0; i--) {
      if(prime[i]) {
        return i;
      }
    }
    return 0;
  }

  /**
   *  Converts a hash code in the range Integer.MIN_VALUE...Integer.MAX_VALUE
   *  to a value in the range 0...(size of hash table) - 1.
   *
   *  This function should have package protection (so we can test it), and
   *  should be used by insert, find, and remove.
   **/

  int compFunction(int code) {
    // Replace the following line with your solution.
    //System.out.println(length);
    return (((3*code + 7) % 16908799) % length);
  }

  /** 
   *  Returns the number of entries stored in the dictionary.  Entries with
   *  the same key (or even the same key and value) each still count as
   *  a separate entry.
   *  @return number of entries in the dictionary.
   **/

  public int size() {
    // Replace the following line with your solution.
    return numOfEntries;
  }

  /** 
   *  Tests if the dictionary is empty.
   *
   *  @return true if the dictionary has no entries; false otherwise.
   **/

  public boolean isEmpty() {
    // Replace the following line with your solution.
    if (numOfEntries == 0) {
      return true;
    }else {
      return false;
    }
  }

  /**
   *  Create a new Entry object referencing the input key and associated value,
   *  and insert the entry into the dictionary.  Return a reference to the new
   *  entry.  Multiple entries with the same key (or even the same key and
   *  value) can coexist in the dictionary.
   *
   *  This method should run in O(1) time if the number of collisions is small.
   *
   *  @param key the key by which the entry can be retrieved.
   *  @param value an arbitrary object.
   *  @return an entry containing the key and value.
   **/

  public Entry insert(Object key, Object value) {
    // Replace the following line with your solution.
    Entry added = new Entry();
    added.key = key;
    added.value = value;

    int hashCodeOfAdded = (added.key).hashCode();
    //System.out.println(hashCodeOfAdded);
    int bucketIndex = Math.abs(compFunction(hashCodeOfAdded));

    //System.out.println(bucketIndex);

    if (bucketsArray[bucketIndex] == null) {
      bucketsArray[bucketIndex] = new DList();
      bucketsArray[bucketIndex].insertBack(added);
      numOfEntries++;
      //System.out.println("i just add: "+added);
      //System.out.println(numOfEntries);
    }else {
      numOfCollisions++;
      bucketsArray[bucketIndex].insertBack(added);
      numOfEntries++;
      //System.out.println("i just add: "+added);
      //System.out.println(numOfEntries);
    }
    return added;
  }

  /** 
   *  Search for an entry with the specified key.  If such an entry is found,
   *  return it; otherwise return null.  If several entries have the specified
   *  key, choose one arbitrarily and return it.
   *
   *  This method should run in O(1) time if the number of collisions is small.
   *
   *  @param key the search key.
   *  @return an entry containing the key and an associated value, or null if
   *          no entry contains the specified key.
   **/

  public Entry find(Object key) {
    // Replace the following line with your solution.
    int hashCodeOfTarget = key.hashCode();
    int indexOfTarget = Math.abs(compFunction(hashCodeOfTarget));

    if (bucketsArray[indexOfTarget] == null || bucketsArray[indexOfTarget].length() == 0) {
      return null;
    }else {
      DListNode targetNode = ((DListNode) ((bucketsArray[indexOfTarget]).front()));
      for (int i = 0; i < bucketsArray[indexOfTarget].length(); i++) { 
        try {
          if ((((Entry) (targetNode.item())).key()).equals(key)) {
            //System.out.println("i find: "+((Entry) (targetNode.item())).key()+", its value is: "+((Entry) (targetNode.item())).value());
            return ((Entry) (targetNode.item()));
          }else {
            targetNode = ((DListNode) (targetNode.next()));
          }
        }catch (InvalidNodeException e1) {
          System.out.println("in 'find', i got a exception");
        } 
      }
      //System.out.println("I find nothing");
      return null;
    }
  }

  /** 
   *  Remove an entry with the specified key.  If such an entry is found,
   *  remove it from the table and return it; otherwise return null.
   *  If several entries have the specified key, choose one arbitrarily, then
   *  remove and return it.
   *
   *  This method should run in O(1) time if the number of collisions is small.
   *
   *  @param key the search key.
   *  @return an entry containing the key and an associated value, or null if
   *          no entry contains the specified key.
   */

  public Entry remove(Object key) {
    // Replace the following line with your solution.
    int hashCodeOfRemoved = key.hashCode();
    int indexOfRemoved = Math.abs(compFunction(hashCodeOfRemoved));

    if (bucketsArray[indexOfRemoved] == null || bucketsArray[indexOfRemoved].length() == 0) {
      //System.out.println("nothing's here");
      return null;
    }else {
      DListNode removedNode = ((DListNode) ((bucketsArray[indexOfRemoved]).front()));
      for (int i = 0; i < bucketsArray[indexOfRemoved].length(); i++) { 
        try {
          if ((((Entry) (removedNode.item())).key()).equals(key)) {
            Entry removedEntry = (Entry) removedNode.item();
            //System.out.println("i just removed: "+(removedEntry.key()));
            removedNode.remove();
            numOfEntries--;
            //System.out.println(numOfEntries);

            return removedEntry;
          }else {
            if (removedNode.isValidNode()) {
              removedNode = ((DListNode) (removedNode.next()));
            }else {
              return null;
            }  
          }
        }catch (InvalidNodeException e1) {
          System.out.println("in 'remove', i got a exception");
        } 
      }
      return null;
    }
  }

  /**
   *  Remove all entries from the dictionary.
   */
  public void makeEmpty() {
    // Your solution here.
    bucketsArray = new DList[length];
    numOfEntries = 0;
  }

  public int numOfCollisions() {
    return numOfCollisions;
  }

  public static void main(String[] argv) {
      HashTableChained table = new HashTableChained();
      System.out.println("should be 0: "+table.size());
      System.out.println("should be true: "+table.isEmpty());

      table.insert("WTF", "LOL");
      table.insert("CAT", "MEOW");
      table.insert("DOG", "BARK");
      table.insert("HUMAN", "WHYYY");
      table.insert(10, 10);

      System.out.println("collisions should be 0: "+table.numOfCollisions());

      Entry a = table.find("WTF");
      Entry b = table.find("CAT");
      table.remove("DOG");
      Entry c = table.find("DOG");

      System.out.println("Should be 4: "+table.size());

      table.insert("LEAGUE","HOOKED");
      table.insert("DRAKE","A+");
      table.insert("DRAKE","A+");
      System.out.println("collisions should be 1: "+table.numOfCollisions());

      System.out.println("should be 7: "+table.size());
      table.find(10);
      table.find("DRAKE");
      table.remove("DRAKE");
      table.find("DRAKE");
      System.out.println("should be 6: "+table.size());
      table.remove("DRAKE");
      table.remove("DRAKE");
      System.out.println("should be 5: "+table.size());

      table.remove("WTF");
      table.remove("CAT");
      table.remove("HUMAN");
      table.remove(10);
      table.remove("LEAGUE");

      System.out.println(table.bucketsArray[0]);
      System.out.println(table.bucketsArray[1]);
      System.out.println(table.bucketsArray[2]);
      System.out.println(table.bucketsArray[3]);
      System.out.println(table.bucketsArray[4]);
      
      System.out.println("should be 0: "+table.size());
      System.out.println("should be true: "+table.isEmpty());



      table.makeEmpty();
      System.out.println("this should be true: "+table.isEmpty());
      System.out.println("Should be 0: "+table.size());

      System.out.println(table.bucketsArray[0]);
      System.out.println(table.bucketsArray[1]);
      System.out.println(table.bucketsArray[2]);
      System.out.println(table.bucketsArray[3]);
      System.out.println(table.bucketsArray[4]);
      

      table.remove("DRAKE");
  }
}


