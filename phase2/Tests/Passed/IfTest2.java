package Tests.Passed;

public class IfTest2 {
    int m;

    public IfTest2(int m) {
    }

    IfTest2() {
    }

    int a() {
        return 3;
    }

    int[] b() {
        return new int[] { 1, 2, 3 };
    }
}

class B {
    B() {
        int m = 123;
        IfTest2 x = new IfTest2(m);

        if (m < 10) {
            x.a();
        } else {
            x.b();
        }
    }

    public static void main(String[] args) {

    }
}