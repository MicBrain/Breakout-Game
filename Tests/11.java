/* SimpleBoard.java */

/**
 *  Simple class that implements an 8x8 game board with three possible values
 *  for each cell:  0, 1 or 2.
 *
 *  DO NOT CHANGE ANY PROTOTYPES IN THIS FILE.
 **/

public class SimpleBoard {
  private final static int DIMENSION = 8;
  private int[][] grid;

  /**
   *  Invariants:  
   *  (1) grid.length == DIMENSION.
   *  (2) for all 0 <= i < DIMENSION, grid[i].length == DIMENSION.
   *  (3) for all 0 <= i, j < DIMENSION, grid[i][j] >= 0 and grid[i][j] <= 2.
   **/

  /**
   *  Construct a new board in which all cells are zero.
   */

  public SimpleBoard() {
    grid = new int[DIMENSION][DIMENSION];
  }

  /**
   *  Set the cell (x, y) in the board to the given value mod 3.
   *  @param value to which the element should be set (normally 0, 1, or 2).
   *  @param x is the x-index.
   *  @param y is the y-index.
   *  @exception ArrayIndexOutOfBoundsException is thrown if an invalid index
   *  is given.
   **/

  public void setElementAt(int x, int y, int value) {
    grid[x][y] = value % 3;
    if (grid[x][y] < 0) {
      grid[x][y] = grid[x][y] + 3;
    }
  }

  /**
   *  Get the valued stored in cell (x, y).
   *  @param x is the x-index.
   *  @param y is the y-index.
   *  @return the stored value (between 0 and 2).
   *  @exception ArrayIndexOutOfBoundsException is thrown if an invalid index
   *  is given.
   */

  public int elementAt(int x, int y) {
    return grid[x][y];
  }

  /**
   *  Returns true if "this" SimpleBoard and "board" have identical values in
   *    every cell.
   *  @param board is the second SimpleBoard.
   *  @return true if the boards are equal, false otherwise.
   */

  public boolean equals(Object board) {
    // Replace the following line with your solution.  Be sure to return false
    //   (rather than throwing a ClassCastException) if "board" is not
    //   a SimpleBoard.
    if (!(board instanceof SimpleBoard)) {
      return false;
    }else {
      for (int i = 0; i < DIMENSION; i++) {
        for (int j = 0; j < DIMENSION; j++) {
          if (((SimpleBoard) board).elementAt(j,i) != this.elementAt(j,i)) {
            return false;
          }
        }
      }
      return true;
    }
  }

  /**
   *  Returns a hash code for this SimpleBoard.
   *  @return a number between Integer.MIN_VALUE and Integer.MAX_VALUE.
   */

  public int hashCode() {
    // Replace the following line with your solution.
    
    int hashVal = 0;
    for (int y = 0; y < DIMENSION; y++) {
      for (int x = 0; x < DIMENSION; x++) {
        if (x+1 < DIMENSION && y+1 < DIMENSION) {
          hashVal += (x+this.elementAt(x,y))*(y) + (x*y)*((x*this.elementAt(x,y))+y) + (x*this.elementAt(x+1,y)) + (y*this.elementAt(x, y+1));
        }else {
          hashVal += (x+this.elementAt(x,y))*(y) + (x*y)*((x*this.elementAt(x,y))+y);
        }
      }
    }
    return hashVal;
    /*
    int hashNum = 0;
    for(int i=0; i<DIMENSION; i++) {
      for(int j=0; j<DIMENSION; j++) {
        hashNum+= (i*1000 + j*100 +10*(i+j)*elementAt(i,j) + (i*j)*(i+j+elementAt(i,j)));
      }
    }
    return hashNum;
    */
  }

  private int numTransferToBase3(int i) {
    if (i < 3) {
      return i;
    }else {
      return i%3 + 10*numTransferToBase3((int) i/3);
    }
  }

}
