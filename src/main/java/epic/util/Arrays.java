package epic.util;

/**
 * @author dlwh
 */
public final class Arrays {
    private Arrays() {}

    public static double[][] newArray(int x, int y) {
        return new double[x][y];
    }

    public static double[][] fillArray(int x, int y, double v) {
        double[][] arr = newArray(x,y);
        for(int i = 0; i < x; i++) {
           java.util.Arrays.fill(arr[i], v);
        }

        return arr;
    }


}
