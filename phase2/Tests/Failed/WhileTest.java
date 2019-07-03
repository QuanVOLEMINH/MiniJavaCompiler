package Tests.Failed;

public class WhileTest {

    public WhileTest(int m) {
    }

    WhileTest() {
    }

    int a() {
        return 3;
    }

}

class B {
    B() {
        WhileTest x = new WhileTest();

        while (x.a()) {
            int m = 123;
        }
    }
}