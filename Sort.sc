// The purpose of Sort is to compare the lengths of sort algorithms in Java
// and Scala, since Scala is meant to be less verbose

// this uses some pretty nifty pattern matching!

object Sort {

  // Let's break it down
  // We're defining bubblesort
  // It's a generic, where type A must be an Ordered value
  // It takes a List of type A as an argument
  // It returns a List of type A
  def bubblesort[A <% Ordered[A]](list: List[A]): List[A] = {

    // We're defining our helper fxn: sort
    // It takes 2 Lists of type A: as and bs
    // It returns another List of type A
    def sort(a: List[A], b: List[A]): List[A] =
      // If as is empty, return Nil
      if (a.isEmpty) b
      // else, bubble as, Nil, and bs
      else bubble(a, Nil, b)


    // Here's our next helper method
    // It takes 3 args, all Lists of type A
    // It returns another List of type A
    // The "a match" means it's a pattern matching function for List a
    def bubble(a: List[A], z: List[A], b: List[A]): List[A] = a match {
      // What does h1 :: h2 :: t mean?
      // [h1, h2, t...] --> a collection of 2 elements, h1 and h2, then any
      // number of other elements, t
      case h1 :: h2 :: t =>
        // if the first el is > the second el
        // then we want to bubble ( [h1, t...], [h2, z...], b )
        if (h1 > h2) bubble(h1 :: t, h2 :: z, b)
        // otherwise, we want to bubble ( [h2, t...], [h1, z...], b)
        else bubble(h2 :: t, h1 :: z, b)
      // if the list is of length 1,
      // run sort on ( z, [h1, b...] )
      case h1 :: Nil => sort(z, h1 :: b)
      case Nil => Nil
    }

    sort(list, Nil)
  }

  // Jova Bubblesort:
  /*
    void bubbleSort(int arr[]) {
      int n = arr.length;
        for (int i = 0; i < n-1; i++)
          for (int j = 0; j < n-i-1; j++)
            if (arr[j] > arr[j+1]) {
              int temp = arr[j];
              arr[j] = arr[j+1];
              arr[j+1] = temp;
            }
    }
   */

  // bubble sort conclusion? Seems to be a tad longer than Java...
  // But how about quicksort?

  def quicksort(arr: List[Int]): List[Int] = {
    if (arr.length <= 1) arr
    else {
      val pivot = arr(arr.length / 2)
      quicksort(arr.filter(_ < pivot)) :::
        arr.filter(_ == pivot) :::
        quicksort(arr.filter(_ > pivot))
    }
  }

  // Java quicksort
  /*
  int partition(int arr[], int low, int high) {
    int pivot = arr[high];
    int i = (low-1); // index of smaller element
    for (int j=low; j<high; j++) {
      if (arr[j] <= pivot) {
        i++;
        int temp = arr[i];
        arr[i] = arr[j];
        arr[j] = temp;
      }
    }
    int temp = arr[i+1];
    arr[i+1] = arr[high];
    arr[high] = temp;
    return i+1;
    }

    void sort(int arr[], int low, int high) {
      if (low < high) {
        int pi = partition(arr, low, high);
        sort(arr, low, pi-1);
        sort(arr, pi+1, high);
      }
    }
   */

  // And how about mergesort?

  def merge(left: List[Int], right: List[Int]): List[Int] = (left, right) match {
    case(left, Nil) => left
    case(Nil, right) => right
    case(leftHead :: leftTail, rightHead :: rightTail) =>
      if (leftHead < rightHead) leftHead::merge(leftTail, right)
      else rightHead :: merge(left, rightTail)
  }

  def mergesort(list: List[Int]): List[Int] = {
    val n = list.length / 2
    if (n == 0) list // i.e. if list is empty or single value, no sorting needed
    else {
      val (left, right) = list.splitAt(n)
      merge(mergesort(left), mergesort(right))
    }
  }

  // Java mergesort
  /*
  void merge(int arr[], int l, int m, int r) {
    int n1 = m - l + 1;
    int n2 = r - m;
    int L[] = new int [n1];
    int R[] = new int [n2];
    for (int i=0; i<n1; ++i)
        L[i] = arr[l + i];
    for (int j=0; j<n2; ++j)
        R[j] = arr[m + 1+ j];
    int i = 0, j = 0;
    int k = l;
    while (i < n1 && j < n2) {
        if (L[i] <= R[j]) {
            arr[k] = L[i];
            i++;
        }
        else {
            arr[k] = R[j];
            j++;
        }
        k++;
    }
    while (i < n1) {
        arr[k] = L[i];
        i++;
        k++;
    }
    while (j < n2) {
        arr[k] = R[j];
        j++;
        k++;
    }
  }
  void sort(int arr[], int l, int r) {
    if (l < r) {
        int m = (l+r)/2;
        sort(arr, l, m);
        sort(arr , m+1, r);
        merge(arr, l, m, r);
    }
  }
   */

  // In conclusion,
  // There seem to be cases where Scala is plenty better than Java
  // Java mergesort is so verbose and lengthy and hard to understand, whereas
  // the mergesort in scala is readable and simple, given you know a bit of
  // scala. They ARE different ways of thinking: scala is obviously more
  // functional than Java.
  // The quicksort is very simple and easy to understand. You can almost
  // just read it out loud as if it was english!

  def main(args: Array[String]): Unit = {
    println("bubblesort")
    val list0 = List(4, 8, 2, 3, 7, 1, 5, 9, 6)
    println(list0.mkString(", "))
    val newList0 = bubblesort(list0)
    println(newList0.mkString(", "))

    println("\nquicksort")
    var list1 = List(4, 2, 5, 3, 7, 8, 9, 6, 1)
    println(list1.mkString(", "))
    val newList1 = quicksort(list1)
    println(newList1.mkString(", "))

    println("\nmergesort")
    var list2 = List(5, 7, 2, 4, 3, 6, 8, 9, 1)
    println(list2.mkString(", "))
    val newList2 = mergesort(list2)
    println(newList2.mkString(", "))
  }
}









// bean
