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

        {
            int[] arr = new int[] {1,2,3};
            int c = false;
        }
    }

    public static void main(String[] args) {

    }
}