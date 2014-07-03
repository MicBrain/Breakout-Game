/* Entry.java */

package dict;

/**
 *  A class for dictionary entries.
 *
 *  DO NOT CHANGE THIS FILE.  It is part of the interface of the
 *  Dictionary ADT.
 **/

public class Entry {

  protected Object key;
  protected Object value;

  public Object key() {
    return key;
  }

  public Object value() {
    return value;
  }

  //already deleted in the submission version
  public String toString() {
  	String result = "";
  	result += (key + ": " + value);
  	return result;
  }
}
