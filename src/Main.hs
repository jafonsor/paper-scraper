module Main where

import BenjaminsCSV (generateBenjaminsCSV)
import CambridgeCSV (generateCambridgeCSV)

main = generateBenjaminsCSV >> generateCambridgeCSV

