package Tests.Failed;

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
        int c = 0;
        TryCatchTest x = new TryCatchTest(m);
        try {
            c = m + 100;
            c = false;
        } catch (Exception e) {
        } finally {
            c += x.a();
        }
    }

    public static void main(String[] args) {

    }
}