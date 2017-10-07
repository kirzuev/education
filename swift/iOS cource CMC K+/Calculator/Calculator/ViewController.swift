//
//  ViewController.swift
//  Calculator
//
//  Created by Alexandr Poroshin on 04/10/2017.
//  Copyright © 2017 Alexandr Poroshin. All rights reserved.
//

import UIKit

class ViewController: UIViewController {
    
    // implicitly unwrapped optional
    @IBOutlet weak var displayLabel: UILabel!
    
    var isUserTypingToDisplay = false
    
    var firstOperand: Double?
    var function: ((Double, Double) -> Double)?
    
    var displayValue: Double {
        get {
            return Double(displayLabel.text!)!
        }
        set {
            displayLabel.text = String(newValue)
        }
    }
    
    // argument label
    @IBAction func digitPressed(_ sender: UIButton) {
        guard let buttonTitle = sender.currentTitle else {
            return
        }
        
        var currentDisplayText = displayLabel.text ?? ""
        if !isUserTypingToDisplay || currentDisplayText == "0" {
            currentDisplayText = ""
        }
        
        displayLabel.text = currentDisplayText + buttonTitle
        isUserTypingToDisplay = true
        
    }

    // -, *, /, <-, C
    @IBAction func performOperation(_ sender: UIButton) {
        guard let operationSymbol = sender.currentTitle else {
            return
        }
        
        switch operationSymbol {
            case "√":
                displayValue = sqrt(displayValue)
            case "+":
                firstOperand = displayValue
                function = { $0 + $1 }
            case "=":
                if let operand = firstOperand, let operation = function {
                    displayValue = operation(operand, displayValue)
                    firstOperand = nil
                    function = nil
                }
            case "-":
                firstOperand = displayValue
                function = { $0 - $1 }
            case "*":
                firstOperand = displayValue
                function = { $0 * $1 }
            case "/":
                firstOperand = displayValue
                function = { $0 / $1 }
            case "<-":
                if isUserTypingToDisplay && displayLabel.text != "" {
                    var labelText = displayLabel.text!
                    labelText.remove(at: labelText.index(before: labelText.endIndex))
                    if labelText == "" {
                        labelText = "0"
                    }
                    displayLabel.text = labelText
                }
            case "C":
                displayLabel.text = "0"
            default:
                break
        }
        if operationSymbol != "<-" {
            isUserTypingToDisplay = false
        }
    }
    
}

