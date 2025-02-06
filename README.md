# A Domain Specific Language on Financial Contracts
This project is a part of my bachelor thesis at the Department of Computer Science at University of Copenhagen. Grade received was 12.

## Quick Overview
The purpose was to implement a domain-specific language in F# that enables the user to:
* create complex financial contracts in a readable, declarative way using custom data types
* manage the contracts by functions that operate on those data types
* price the contracts using continuous-time finance principles.

The data types are designed so that it is possible to create contracts that are readable for non-programmers, while being able to price those contracts efficiently.

### Example
Define a call option on the AAPL stock, with a $75 strike price, expiring in 10 days:
```
europeanCall 10 "AAPL" 75 USD // simple and declarative!
```

### Documentation
The program is fully documented in the [thesis](https://github.com/KNS00/A-DSL-on-Financial-Contracts/blob/main/thesis.pdf). 

The [slides](https://github.com/KNS00/A-DSL-on-Financial-Contracts/blob/main/presentation.pdf) also provide a detailed introduction for someone comfortable with programming language semantics.

## Prerequisites
To contribute to the project, the following is highly beneficial:
* experience with functional programming, in particular discriminated unions, recursion and higher-order functions.
* familiarity with continuous time finance concepts such as options, geometric brownian motions and monte carlo simulations. If not, these concepts are introduced in the [thesis](https://github.com/KNS00/A-DSL-on-Financial-Contracts/blob/main/thesis.pdf) and [slides](https://github.com/KNS00/A-DSL-on-Financial-Contracts/blob/main/presentation.pdf).



## Getting Started
To get started, 
* clone the project
* ensure that you have [the latest .NET SDK version](https://dotnet.microsoft.com/en-us/download) installed on your system
* Run the project:

  ```
  dotnet build
  dotnet run 
  ```

* Load the solution and define your contracts in a new file or within Program.fs, such as
  ```
  let newBond = zcb 10 1000.0 USD // a zero coupon bond: acquire 1000.0 USD in 10 days
  ```


To learn how to build contracts and price and manage them, read through the [slides](https://github.com/KNS00/A-DSL-on-Financial-Contracts/blob/main/presentation.pdf) of the original defence. They provide a great explanation for doing so.


# Running the results of the thesis
To run the results outlined in the report,

* Build the project with ``dotnet build``
* Run the project with ``dotnet run``

## Running specific sections
If you want to run the results for a specific section, use the commands below:

Section 5:

```
dotnet run section5
```

Section 7:

```
dotnet run section7
```

## Running the unit tests
The project uses xUnit and FsUnit for unit testing. 

To get started with the testing, navigate to the `Tests` folder in the project directory.

### General testing

To run all unit tests: 

Build the project with ``dotnet build``

Run the tests with
``dotnet test``

Output:

```
Passed!  - Failed:     0, Passed:   175, Skipped:     0, Total:   175, Duration: 1 m 19 s - Tests.dll (net7.0)
```

### Running Specific Tests

#### Contract Management Tests

To run only the Contract Management tests: 

```
dotnet test --filter ContractManagement
```

Output:

```
Passed!  - Failed:     0, Passed:    43, Skipped:     0, Total:    43, Duration: 115 ms - Tests.dll (net7.0)
```

#### Pricing Tests

To run the pricing tests, run the following commands:

```
  dotnet test --filter currencyTests
  dotnet test --filter observableTests
  dotnet test --filter contractTests
  dotnet test --filter simulationTests
```

Outputs:

```
- Passed!  - Failed:     0, Passed:     8, Skipped:     0, Total:     8, Duration: 8 ms - Tests.dll (net7.0)
- Passed!  - Failed:     0, Passed:    68, Skipped:     0, Total:    68, Duration: 55 ms - Tests.dll (net7.0)
- Passed!  - Failed:     0, Passed:    40, Skipped:     0, Total:    40, Duration: 16 s - Tests.dll (net7.0)
- Passed!  - Failed:     0, Passed:    13, Skipped:     0, Total:    13, Duration: 57 s - Tests.dll (net7.0)
```
