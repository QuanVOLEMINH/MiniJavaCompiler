package Tests.Failed;

public class ConstructorSignatureNotFoundTest {
    int m;

    public ConstructorSignatureNotFoundTest(int m) {
    }

    ConstructorSignatureNotFoundTest() {
    }

    int a() {
        return 3;
    }

}

class B {
    B() {
        int m = 123;
        ConstructorSignatureNotFoundTest x = new ConstructorSignatureNotFoundTest(m);
        x.a();
        ConstructorSignatureNotFoundTest y = new ConstructorSignatureNotFoundTest(false);
    }

    public static void main(String[] args) {

    }
}