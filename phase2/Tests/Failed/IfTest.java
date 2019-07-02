package Tests.Failed;

public class IfTest {
    int m;

    public IfTest(int m) {
    }

    IfTest() {
    }

    int a() {
        return 3;
    }

}

class B {
    B() {
        int m = 123;
        IfTest x = new IfTest(m);

        if (m = 10) {
            x.a();
        }
    }

    public static void main(String[] args) {

    }
}