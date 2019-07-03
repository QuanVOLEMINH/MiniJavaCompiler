package Tests.Passed;

public class IfTest1 {
    int m;

    public IfTest1(int m) {
    }

    IfTest1() {
    }

    int a() {
        return 3;
    }

}

class B {
    B() {
        int m = 123;
        IfTest1 x = new IfTest1(m);

        if (true) {
            x.a();
        }
    }

    public static void main(String[] args) {

    }
}