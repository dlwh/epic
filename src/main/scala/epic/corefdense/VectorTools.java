package epic.corefdense;

import java.util.Random;
import java.util.Vector;

/**
 * gdurrett: taken from https://code.google.com/p/wordsimilarity/
 * 
 * Vector manipulation tools.
 * READY
 * @author Brent Kievit-Kylar
 */

public class VectorTools {

  // Randomness.
  public static Random rand = new Random();

  public static double[] toDouble(int[] vals) {
    double[] ret = new double[vals.length];
    for(int i=0;i<vals.length;i++) {
      ret[i] = vals[i];
    }
    return ret;
  }

  /**
   * Create a zeroed array.
   * @param len
   * @return
   */
  public static double[] zero(int len) {
    double[] ret = new double[len];
    for(int i=0;i<len;i++) {
      ret[i] = 0;
    }
    return ret;
  }

  public static double[][] ones(int len, int width) {
    double[][] ret = new double[len][width];
    for(int x=0;x<len;x++) {
      for(int y=0;y<width;y++) {
        ret[x][y] = 1;
      }
    }
    return ret;
  }

  public static double[][] ns(int len, int width, double n) {
    double[][] ret = new double[len][width];
    for(int x=0;x<len;x++) {
      for(int y=0;y<width;y++) {
        ret[x][y] = n;
      }
    }
    return ret;
  }

  public static double[][] pow(double[][] from, double power) {
    double[][] ret = new double[from.length][from[0].length];
    for(int x=0;x<from.length;x++) {
      for(int y=0;y<from[0].length;y++) {
        ret[x][y] = Math.pow(from[x][y], power);
      }
    }
    return ret;
  }

  /**
   * Pointwise multiplication of two vectors.
   * @param v1
   * @param v2
   * @return
   */
  public static double[] getPointwiseMultiply(double[] v1, double[] v2) {
    double[] ret = new double[v1.length];
    for(int i=0;i<v1.length;i++) {
      ret[i] = v1[i] * v2[i];
    }
    return ret;
  }

  /**
   * Sum the entire set of vectors.
   * @param vectors       Set of vectors.
   * @return
   */
  public static double[] sumVectors(Vector<double[]> vectors) {
    if(vectors.size() <= 0) {
      return null;
    }

    double[] ret = new double[vectors.get(0).length];
    for(double[] vec : vectors) {
      for(int i=0;i<ret.length;i++) {
        ret[i] += vec[i];
      }
    }
    return ret;
  }

  /**
   * Shift elements in vector times to the right with rotation at the ends.
   * @param v
   * @param times
   * @return
   */
  public static double[] rotate(double[] v, int times) {
    double[] ret = new double[v.length];
    for(int i=0;i<v.length;i++) {

      // We need to find the appropriate modulus index.
      // This fixes the inapropriate negative modulus problem.
      int index = (i + times) % v.length;
      if(index < 0) {
        index += v.length;
      }

      ret[i] = v[index];
    }
    return ret;
  }

  /**
   * Print an array of doubles.
   * @param a
   */
  public static void show(double[] a) {
    for(double d : a) {
      System.out.print("[" + d + "]");
    }
    System.out.println();
  }

  public static void show(int[] a) {
    for(int d : a) {
      System.out.print("[" + d + "]");
    }
    System.out.println();
  }

  public static void show(double[][] a) {
    for(int x=0;x<a.length;x++) {
      for(int y=0;y<a[0].length;y++) {
        System.out.print("[" + a[x][y] + "]");
      }
      System.out.println();
    }
    System.out.println();
  }

  /**
   * Limit the length of the vector to be len if it is greater.
   * @param v
   * @param len
   * @return
   */
  public static double[] limitSize(double[] v, double len) {
    double dist = dist(v);
    if(dist > len) {
      return mult(v,len/dist);
    }
    return mult(v,1.0);
  }

  /**
   * Move a point a fraction of the way from one point to another.
   * @param from
   * @param to
   * @param amount
   */
  public static void setBetween(double[] from, double[] to, double amount) {
    for(int i=0;i<from.length;i++) {
      from[i] = from[i] * (1-amount) + to[i] * amount;
    }
  }

  /**
   * Get the vector that is a fraction of the way between two points.
   * @param from
   * @param to
   * @param amount
   * @return
   */
  public static double[] getBetween(double[] from, double[] to, double amount) {
    double[] ret = new double[from.length];
    for(int i=0;i<from.length;i++) {
      ret[i] = from[i] * (1-amount) + to[i] * amount;
    }
    return ret;
  }

  /**
   * Add the first vector to the second one.
   * @param from
   * @param to
   */
  public static void setAdd(double[] from, double[] to) {
    for(int i=0;i<from.length;i++) {
      to[i] += from[i];
    }
  }

  /**
   * Get the result of adding two vectors.
   * @param from
   * @param to
   * @return
   */
  public static double[] getAdd(double[] from, double[] to) {
    double[] ret = new double[from.length];
    for(int i=0;i<from.length;i++) {
      ret[i] = from[i] + to[i];
    }
    return ret;
  }

  public static double[][] getAdd(double[][] from, double[][] to) {
    double[][] ret = new double[from.length][from[0].length];
    for(int x=0;x<from.length;x++) {
      for(int y=0;y<from[0].length;y++) {
        ret[x][y] = from[x][y] + to[x][y];
      }
    }
    return ret;
  }

  public static double[][] getAdd(double[][] from, double to) {
    double[][] ret = new double[from.length][from[0].length];
    for(int x=0;x<from.length;x++) {
      for(int y=0;y<from[0].length;y++) {
        ret[x][y] = from[x][y] + to;
      }
    }
    return ret;
  }

  public static double[][] sign(double[][] from) {
    double[][] ret = new double[from.length][from[0].length];
    for(int x=0;x<from.length;x++) {
      for(int y=0;y<from[0].length;y++) {
        ret[x][y] = Math.signum(from[x][y]);
      }
    }
    return ret;
  }

  public static double[][] notEqual(double[][] from, double[][] to) {
    double[][] ret = new double[from.length][from[0].length];
    for(int x=0;x<from.length;x++) {
      for(int y=0;y<from[0].length;y++) {
        double dist = from[x][y] - to[x][y];
        if(dist > .1 || dist < -.1) {
          ret[x][y] = 1;;
        } else {
          ret[x][y] = 0;
        }
      }
    }
    return ret;
  }

  public static double[][] equal(double[][] from, double[][] to) {
    double[][] ret = new double[from.length][from[0].length];
    for(int x=0;x<from.length;x++) {
      for(int y=0;y<from[0].length;y++) {
        double dist = from[x][y] - to[x][y];
        if(dist > .1 || dist < -.1) {
          ret[x][y] = 0;
        } else {
          ret[x][y] = 1;
        }
      }
    }
    return ret;
  }

  /**
   * Set this vector to the result of subtracting the other.
   * @param from
   * @param to
   */
  public static void setSub(double[] from, double[] to) {
    for(int i=0;i<from.length;i++) {
      to[i] -= from[i];
    }
  }

  /**
   * Transpose matrix.
   * @param vecs
   * @return
   */
  public static double[][] transpose(double[][] vecs) {
    double[][] t = new double[vecs[0].length][vecs.length];
    for(int x=0;x<vecs.length;x++) {
      for(int y=0;y<vecs[0].length;y++) {
        t[y][x] = vecs[x][y];
      }
    }
    return t;
  }

  public static double[][] transpose(double[] vecs) {
    double[][] t = new double[1][vecs.length];
    for(int x=0;x<vecs.length;x++) {
      t[0][x] = vecs[x];
    }
    return t;
  }

  public static double[][] to2D(double[] vecs) {
    double[][] t = new double[vecs.length][1];
    for(int x=0;x<vecs.length;x++) {
      t[x][0] = vecs[x];
    }
    return t;
  }

  /**
   * Calculate the covariance matrix.
   * @param a
   * @return
   */
  public static double[][] cov(double[][] a) {
    return mult(a,transpose(a));
  }

  /**
   * Calculate the dominant eigenvalue.
   * @param vec
   * @return
   */
  public static double[] domEig(double[][] vec) {
    double[][] bt = {newGaussian(vec[0].length)};
    double[][] b = transpose(bt);

    double[][] a = vec;
    for(int i=0;i<100;i++) {
      a = mult(a,vec);
      double[][] eig = mult(a,b);
      for(int j=0;j<eig.length;j++) {
        System.out.print(eig[j][0]);
      }
      System.out.println();
    }
    return null;
  }

  /**
   * Matricies are x, by y
   * Matrix.length = height, Matrix[0].length = width
   * @param a
   * @param b
   * @return
   */
  public static double[][] mult(double[][] a, double[][] b) {
    double[][] c = new double[a.length][b[0].length];
    for(int x=0;x<c.length;x++) {
      for(int y=0;y<c[0].length;y++) {
        double sum = 0;
        for(int i=0;i<a[0].length;i++) {
          sum += a[x][i] * b[i][y];
        }
        c[x][y] = sum;
      }                       
    }
    return c;
  }

  /**
   * Subtract the means from the collumns.
   * @param vecs
   * @return
   */
  public static double[][] getSubMean(double[][] vecs) {
    return getSubMean(vecs,mean(vecs));
  }

  /**
   * Subtract the means from the collumns.
   * @param vecs
   * @return
   */
  private static double[][] getSubMean(double[][] vecs, double[] mean) {
    double[][] subMean = new double[vecs.length][];

    // Subtract from each entry.
    int i = 0;
    for(double[] vec : vecs) {
      subMean[i] = VectorTools.getSub(mean, vec);
      i++;
    }

    return subMean;
  }

  /**
   * Calculate the collumn means.
   * @param vecs
   * @return
   */
  public static double[] mean(double[][] vecs) {
    double[] mean = new double[vecs[0].length];
    for(double[] entry : vecs) {
      VectorTools.setAdd(entry, mean);
    }
    mean = VectorTools.mult(mean, 1.0/vecs.length);
    return mean;
  }

  /**
   * Subtract everything in the first vector from the second.
   * @param a
   * @param b
   * @return
   */
  public static double[] getSub(double[] a, double[] b) {
    double[] ret = new double[a.length];
    for(int i=0;i<a.length;i++) {
      ret[i] = a[i] - b[i];
    }
    return ret;
  }

  public static double[][] getSub(double[][] a, double[][] b) {
    double[][] ret = new double[a.length][a[0].length];
    for(int x=0;x<a.length;x++) {
      for(int y=0;y<a[0].length;y++) {
        ret[x][y] = a[x][y] - b[x][y];
      }
    }
    return ret;
  }

  public static double[][] addAsRow(double[][] a, double[][] b) {
    double[][] ret = new double[a.length][a[0].length];
    for(int x=0;x<a.length;x++) {
      for(int y=0;y<a[0].length;y++) {
        ret[x][y] = a[x][y] + b[0][y];
      }
    }
    return ret;
  }

  public static double[][] subAsRow(double[][] a, double[][] b) {
    double[][] ret = new double[a.length][a[0].length];
    for(int x=0;x<a.length;x++) {
      for(int y=0;y<a[0].length;y++) {
        ret[x][y] = a[x][y] - b[0][y];
      }
    }
    return ret;
  }

  public static double[][] addAsCollumn(double[][] a, double[][] b) {
    double[][] ret = new double[a.length][a[0].length];
    for(int x=0;x<a.length;x++) {
      for(int y=0;y<a[0].length;y++) {
        ret[x][y] = a[x][y] + b[x][0];
      }
    }
    return ret;
  }

  public static double[][] subAsCollumn(double[][] a, double[][] b) {
    double[][] ret = new double[a.length][a[0].length];
    for(int x=0;x<a.length;x++) {
      for(int y=0;y<a[0].length;y++) {
        ret[x][y] = a[x][y] - b[x][0];
      }
    }
    return ret;
  }

  /**
   * Distance between two vectors.
   * @param a
   * @param b
   * @return
   */
  public static double dist(double[] a, double[] b) {
    double sum = 0;
    for(int i=0;i<a.length;i++) {
      sum += (a[i] - b[i]) * (a[i] - b[i]);
    }
    return Math.sqrt(sum);
  }

  /**
   * Length of vector.
   * @param a
   * @return
   */
  public static double dist(double[] a) {
    double sum = 0;
    for(int i=0;i<a.length;i++) {
      sum += (a[i] * a[i]);
    }
    return Math.sqrt(sum);
  }

  /**
   * Dot product between two vectors.
   * @param a
   * @param b
   * @return
   */
  public static double dot(double[] a, double[] b) {
    double sum = 0;
    for(int i=0;i<a.length;i++) {
      sum += a[i] * b[i];
    }
    return sum;
  }

  /**
   * Multiply each point in the vector by b.
   * @param a
   * @param b
   * @return
   */
  public static double[] mult(double[] a, double b) {
    double[] ret = new double[a.length];
    for(int i=0;i<a.length;i++) {
      ret[i] = a[i] * b;
    }
    return ret;
  }

  /**
   * Multiply each point in the vector by b.
   * @param a
   * @param b
   * @return
   */
  public static double[][] mult(double[][] a, double b) {
    double[][] ret = new double[a.length][a[0].length];
    for(int x=0;x<a.length;x++) {
      for(int y=0;y<a[0].length;y++) {
        ret[x][y] = a[x][y] * b;
      }
    }
    return ret;
  }

  public static double[][] log(double[][] a) {
    double[][] ret = new double[a.length][a[0].length];
    for(int x=0;x<a.length;x++) {
      for(int y=0;y<a[0].length;y++) {
        ret[x][y] = Math.log(a[x][y]);
      }
    }
    return ret;
  }

  public static double[][] pointMult(double[][] a, double[][] b) {
    double[][] ret = new double[a.length][a[0].length];
    for(int x=0;x<a.length;x++) {
      for(int y=0;y<a[0].length;y++) {
        ret[x][y] = a[x][y] * b[x][y];
      }
    }
    return ret;
  }

  public static double[][] pointDiv(double[][] a, double[][] b) {
    double[][] ret = new double[a.length][a[0].length];
    for(int x=0;x<a.length;x++) {
      for(int y=0;y<a[0].length;y++) {
        ret[x][y] = a[x][y] / b[x][y];
      }
    }
    return ret;
  }

  public static double[][] max(double[][] a, double b) {
    double[][] ret = new double[a.length][a[0].length];
    for(int x=0;x<a.length;x++) {
      for(int y=0;y<a[0].length;y++) {
        ret[x][y] = Math.max(a[x][y], b);
      }
    }
    return ret;
  }

  public static double[][] randn(int l, int w) {
    Random rand = new Random();
    double[][] ret = new double[l][w];
    for(int x=0;x<l;x++) {
      for(int y=0;y<w;y++) {
        ret[x][y] = rand.nextGaussian();
      }
    }
    return ret;
  }


  public static double sum(double[][] a) {
    double sum = 0;
    for(int x=0;x<a.length;x++) {
      for(int y=0;y<a[0].length;y++) {
        sum += a[x][y];
      }
    }
    return sum;
  }

  public static double[] sumRow(double[][] a) {
    double[] sum = new double[a.length];
    for(int x=0;x<a.length;x++) {
      for(int y=0;y<a[0].length;y++) {
        sum[x] += a[x][y];
      }
    }
    return sum;
  }

  public static double[][] diag(int size) {
    double[][] ret = new double[size][size];
    for(int i=0;i<size;i++) {
      ret[i][i] = 1;
    }
    return ret;
  }

  public static double[][] diag(double[] a) {
    double[][] ret = new double[a.length][a.length];
    for(int i=0;i<a.length;i++) {
      ret[i][i] = a[i];
    }
    return ret;
  }

  /**
   * Normalize vector to 1.
   * @param a
   * @return
   */
  public static double[] normalize(double[] a) {
    double dist = dist(a);
    if(dist == 0) {
      return mult(a,0);
    }
    return mult(a,1/dist);
  }

  public static double[][] dists(double[][] a) {
    double[][] ret = new double[a.length][a.length];
    for(int x=0;x<ret.length;x++) {
      for(int y=x+1;y<ret.length;y++) {
        ret[x][y] = ret[y][x] = VectorTools.dot(VectorTools.normalize(a[x]),VectorTools.normalize(a[y]));
      }       
    }
    return ret;
  }

  /**
   * Set the length of the vector.
   * @param a
   * @param len
   * @return
   */
  public static double[] setLen(double[] a, double len) {
    double dist = dist(a);
    return mult(a,len/dist);
  }

  /**
   * Cosine between two vectors.
   * @param a
   * @param b
   * @return
   */
  public static double getCosine(double[] a, double[] b) {
    double sum = dist(a) * dist(b);
    if(sum == 0) {
      return -1;
    }
    return Math.acos(dot(a,b) / sum);
  }

  /**
   * Perform convolution of vector a with b.
   * @param a
   * @param b
   * @return
   */
  public static double[] convolve(double[] a, double[] b) {
    double[] ret = new double[a.length];

    for(int i=0;i<a.length;i++) {
      ret[i] = 0;
      for(int j=0;j<a.length;j++) {
        ret[i] += a[j] * b[(i-j+2*a.length) % a.length];
      }
    }

    return ret;
  }

  /**
   * Undo the convolution between a and b with corelation.
   * @param a
   * @param b
   * @return
   */
  public static double[] corelate(double[] a, double[] b) {
    double[] ret = new double[a.length];

    for(int i=0;i<a.length;i++) {
      ret[i] = 0;
      for(int j=0;j<a.length;j++) {
        ret[i] += a[j] * b[(i+j+2*a.length) % a.length];
      }
    }

    return ret;
  }

  /**
   * Random gaussian vector.
   * @param len
   * @return
   */
  public static double[] newGaussian(int len) {
    double[] ret = new double[len];
    for(int i=0;i<len;i++) {
      ret[i] = rand.nextGaussian();
    }
    return ret;
  }

  /**
   * Random indicator vector of 0 or 1s.
   * @param len
   * @return
   */
  public static double[] newIndicator(int len) {
    double[] ret = new double[len];
    for(int i=0;i<len;i++) {
      ret[i] = rand.nextInt(2) - 1;
    }
    return ret;
  }

  /**
   * Rearange so elements in a are ordered by b.
   * @param a
   * @param b
   * @return
   */
  public static double[] rearangeForward(double[] a, int[] b) {
    double[] ret = new double[a.length];
    for(int i=0;i<a.length;i++) {
      ret[i] = a[b[i]];
    }
    return ret;
  }

  /**
   * Undo the rearange forward.
   * @param a
   * @param b
   * @return
   */
  public static double[] rearangeBackward(double[] a, int[] b) {
    double[] ret = new double[a.length];
    for(int i=0;i<a.length;i++) {
      ret[b[i]] = a[i];
    }
    return ret;
  }

  /**
   * Get a random integer ordering.
   * @param size
   * @return
   */
  public static int[] getRandomOrder(int size) {
    int[] ret = new int[size];
    int[] b = new int[size];

    for(int i=0;i<size;i++) {
      b[i] = i;
    }

    for(int i=0;i<size;i++) {
      int j = rand.nextInt(size - i);
      ret[i] = b[j];
      b[j] = b[size-i-1];
    }

    return ret;
  }
}