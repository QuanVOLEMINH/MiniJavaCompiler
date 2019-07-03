package Tests.Passed;

public class BlockTest {
    int m;

    public BlockTest(int m) {
        this.m = m;
    }

    BlockTest() {
    }

    int a() {
        return 3;
    }

}

class B1 {
    B1() {
        int m = 123;
        BlockTest x = new BlockTest(m);

        if (m < 125) {
            x.a();
        }

        {
            int[] arr = new int[] {1,2,3};
            boolean c = false;
        }
        {;}
    }

    public static void main(String[] args) {

    }
}