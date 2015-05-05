package epic.dense;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.InputStream;

/**
 * Utilities from 
 * https://gist.github.com/ansjsun/6304960
 * 
 * @author gdurrett
 *
 */
public class Word2VecUtils {
  
  private static final int MAX_SIZE = 50;
  
  public static String readString(DataInputStream dis) throws IOException {
    byte[] bytes = new byte[MAX_SIZE];
    byte b = dis.readByte();
    int i = -1;
    StringBuilder sb = new StringBuilder();
    while (b != 32 && b != 10) {
      i++;
      bytes[i] = b;
      b = dis.readByte();
      if (i == 49) {
        sb.append(new String(bytes));
        i = -1;
        bytes = new byte[MAX_SIZE];
      }
    }
    sb.append(new String(bytes, 0, i + 1));
    return sb.toString();
  }

  public static float readFloat(InputStream is) throws IOException {
    byte[] bytes = new byte[4];
    is.read(bytes);
    return getFloat(bytes);
  }

  public static float getFloat(byte[] b) {
    int accum = 0;
    accum = accum | (b[0] & 0xff) << 0;
    accum = accum | (b[1] & 0xff) << 8;
    accum = accum | (b[2] & 0xff) << 16;
    accum = accum | (b[3] & 0xff) << 24;
    return Float.intBitsToFloat(accum);
  }
}
