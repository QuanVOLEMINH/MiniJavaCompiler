package Tests.Failed;

public class ConstructorVarDeclarationTest {
    int m;

    public ConstructorVarDeclarationTest(int m) {
    }

    ConstructorVarDeclarationTest() {
    }

    int a() {
        return 3;
    }

}

class B {
    B() {
        int m = 123;
        ConstructorVarDeclarationTest x = new ConstructorVarDeclarationTest(m);
        x.a();
        int y = true;
    }

    public static void main(String[] args) {

    }
}