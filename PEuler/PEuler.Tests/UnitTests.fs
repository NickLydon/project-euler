namespace PEuler.Tests

open NUnit
open NUnit.Framework
open PEuler

[<TestFixture>]
type myFixture() =

    let sut = new Euler()

    [<Test>]
    member this.ArePrimes([<Values(2,3,5,7,11,23,26681)>] prime) =
        Assert.IsTrue(sut.IsPrime(prime));

    [<Test>]
    member this.AreNotPrimes([<Values(-1,0,1,6,9,21,300,500534)>] prime) =
        Assert.IsFalse(sut.IsPrime(prime));

    [<Test>]
    member this.PrimesUpToAndIncludingX() =
        CollectionAssert.AreEquivalent([2; 3; 5; 7; 11; 13; 17; 19; 23], sut.FindPrimes(24));

    [<Test>]
    member this.Problem10() =
        Assert.AreEqual(142913828922M, sut.Problem10())

        
    [<Test>]
    member this.Problem16() =
        Assert.AreEqual(1366, sut.Problem16())
    
    [<Test>]
    member this.Problem17() =
        Assert.AreEqual(70600674, sut.Problem17())

         
    [<Test>]
    member this.Problem12() =
        Assert.AreEqual(76576500, sut.Problem12())

               
    [<Test>]
    member this.Problem13() =
        Assert.AreEqual(5537376230M, sut.Problem13())

    [<Test>]
    member this.Problem14() =
        Assert.AreEqual(837799, sut.Problem14())

    [<Test>]
    member this.Problem15() =
        Assert.AreEqual(137846528820L, sut.Problem15())

    
    [<Test>]
    member this.Problem19() =
        Assert.AreEqual(171, sut.Problem19())

    [<Test>]
    member this.Problem20() =
        Assert.AreEqual(648, sut.Problem20())

    [<Test>]
    member this.Problem21() =
        Assert.AreEqual(31626, sut.Problem21())

    [<Test>]
    member this.Problem25() =
        Assert.AreEqual(4782, sut.Problem25())
