class SwizzleManager {
  final int a;

  SwizzleManager(int a) {
    this.a = a;
  }

  void print0() {
    System.out.println(a);
  }

  void print1() {
    System.out.println("1"+a);
  }

  void print2() {
    System.out.println("2"+a);
  }
}
