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
    
    let calculator = Calculator()
    
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
        
        if operationSymbol != "C" && operationSymbol != "<-" {
        
            if isUserTypingToDisplay {
                calculator.setOperand(displayValue)
                isUserTypingToDisplay = false
            }
        
            calculator.performOperation(operationSymbol)
        
            if let result = calculator.result {
                displayValue = result
            }

        } else {
            
            switch operationSymbol {
            case "C":
                displayLabel.text = "0"
            case "<-":
                if isUserTypingToDisplay && displayLabel.text != "" {
                    var labelText = displayLabel.text!
                    labelText.remove(at: labelText.index(before: labelText.endIndex))
                    if labelText == "" {
                        labelText = "0"
                    }
                    displayLabel.text = labelText
                }
            default:
                break
            }
            
            calculator.setOperand(displayValue)
            
            if operationSymbol != "<-" {
                isUserTypingToDisplay = false
            }
            
        }

    }
    
}

