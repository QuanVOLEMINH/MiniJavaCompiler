package Tests;

public class A {
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
        x.a();

        int[] data = new int[] { 10, 20, 30, 40, 50, 60, 71, 80, 90, 91 };
        int[] arr = new int[3] { 1, 2, 3 };
    }

    public static void main(String[] args) {

    }
}