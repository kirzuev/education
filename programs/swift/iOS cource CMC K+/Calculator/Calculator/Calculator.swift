//
//  Calculator.swift
//  Calculator
//
//  Created by Alexandr Poroshin on 11/10/2017.
//  Copyright © 2017 Alexandr Poroshin. All rights reserved.
//

import Foundation

class Calculator {
    private var storedValue: Double?
    private var currentWaitingBinaryOperation: WaitingBinaryOperation?
    
    var result: Double? {
        get {
            return storedValue
        }
    }

//    enum Optional<T> {
//        case none
//        case some(T)
//    }
    
    // copy-on-write
    // Value & reference type
    struct WaitingBinaryOperation {
        var firstOperand: Double
        let function: (Double, Double) -> Double
        
        func perform(with secondOperand: Double) -> Double {
            return function(firstOperand, secondOperand)
        }
        
        mutating func setOperand(_ operand: Double) {
            firstOperand = operand
        }
    }
    
    // π
    enum Operation {
        // associated value
        case unaryOperation((Double) -> Double)
        case binaryOperation((Double, Double) -> Double)
        case equals
        case const(Double)
    }
    // operations (+-)
    let operations: Dictionary<String, Operation> = [
        "√" : Operation.unaryOperation(sqrt),
        "+" : Operation.binaryOperation { $0 + $1 },
        "-" : Operation.binaryOperation { $0 - $1 },
        "*" : Operation.binaryOperation { $0 * $1 },
        "/" : Operation.binaryOperation { $0 / $1 },
        "=" : Operation.equals,
        "π" : Operation.const(Double.pi)
    ]
    
    func performOperation(_ symbol: String) {
        guard let operation = operations[symbol] else {
            return
        }
        
        switch operation {
        case .unaryOperation(let function):
            if let operand = storedValue {
                storedValue = function(operand)
            }
        case .binaryOperation(let function):
            if let firstOperand = storedValue {
                currentWaitingBinaryOperation = WaitingBinaryOperation(firstOperand: firstOperand,
                                                                       function: function)
                storedValue = nil
            }
        case .equals:
            if let waitingBinaryOperation = currentWaitingBinaryOperation,
                let secondOperand = storedValue {
                storedValue = waitingBinaryOperation.perform(with: secondOperand)
                currentWaitingBinaryOperation = nil
            }
        case .const(let value):
            storedValue = value
        }
    }
    
    func setOperand(_ operand: Double) {
        storedValue = operand
    }
}
