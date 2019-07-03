package Tests.Failed;

public class ThrowTest {
    int m;

    public ThrowTest(int m) {
    }

    ThrowTest(double a) {
    }

    ThrowTest() {

    }

    int a() {
        return 3;
    }

}

class B {
    B() throws Exception {
        int m = 123;
        ThrowTest x = new ThrowTest(m);

        if (m > 12)
            throw new Exception("asdas");

        boolean b = x.a();

    }

    public static void main(String[] args) {

    }
}