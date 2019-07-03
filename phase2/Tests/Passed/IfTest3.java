package Tests.Passed;

class A {
    public final static int X = 100;
    int m;

    public A(int m) {
    }

    A() {
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
        float f = 5.0f;
        A x = new A(m);

        if ((m < 10) && (f > 1) ) {
            x.a();
        } else {
            x.b();
        }
    }

    public static void main(String[] args) {

    }
}