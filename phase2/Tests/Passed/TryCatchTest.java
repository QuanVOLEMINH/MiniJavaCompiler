package Tests.Passed;

public class TryCatchTest {
    int m;

    public TryCatchTest(int m) {
        this.m = m;
    }

    TryCatchTest() {
    }

    int a() {
        return 3;
    }
}

class B {
    B() {
        int m = 123;
        TryCatchTest x = new TryCatchTest(m);
        int c = 0;
        try {
            c = m + 100 + x.a();
        } catch (Exception e) {
        } finally {
            c += x.a();
        }
    }

    public static void main(String[] args) {

    }
}