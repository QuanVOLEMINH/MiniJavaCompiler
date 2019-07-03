package Tests.Passed;

public class WhileTest {
    int m;

    public WhileTest(int m) {
        this.m = m;
    }

    WhileTest() {
    }

    int a() {
        return 3;
    }

}

class B3 {
    B3() {
        int m = 123;

        while (true) {
            m = 2;
        }
    }

    public static void main(String[] args) {

    }
}