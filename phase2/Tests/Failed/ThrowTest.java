public class A {
    int m;

    public A(int m) {
    }

    A(double a) {
    }

    A(){

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

        if (true)
        	throw new A();

    }

    public static void main(String[] args) {

    }
}