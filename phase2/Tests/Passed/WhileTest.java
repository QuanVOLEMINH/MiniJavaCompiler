public class A {
    int m;

    public A(int m) {
        this.m = m;
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

        while (true) {
            m = 2;
        }
    }

    public static void main(String[] args) {

    }
}