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
        A x = new A(m);

        while (true) {
            x.a();
        }

        try {
            int c = false;
        } catch (Exception e){
            e.printStackTrace();
        }
    }

    public static void main(String[] args) {

    }
}