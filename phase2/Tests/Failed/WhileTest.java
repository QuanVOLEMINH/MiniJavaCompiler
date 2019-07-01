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
        A x = new A();

        while (x.a()) {
            int m = 123;
        }
    }
}