public class A {
    int m;

    public A(int m) {
    }

    A() {
    }

    int a() {
        return 3;
    }

}

class B {
    B() {
        int m = 123;
        A x = new A(m);

        if (true) {
            x.a();
        }
    }

    public static void main(String[] args) {

    }
}