module Main where

import System.IO (hFlush, stdout)
import Data.List (groupBy, sortOn)
import Data.Function (on)
import System.Directory (doesFileExist)
import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

-- Custom Data Types
data Category = Food | Rent | Entertainment | Income | Other deriving (Show, Read, Eq)

data Transaction = Transaction {
    transactionId :: Int,
    date          :: String, -- Format: "YYYY-MM-DD"
    category      :: Category,
    amount        :: Float
} deriving (Show, Read, Eq)

instance ToRecord Transaction where
    toRecord (Transaction tid d c a) =
        record [toField tid, toField d, toField (show c), toField a]

instance FromRecord Transaction where
    parseRecord v
        | V.length v == 4 =
            Transaction <$> v .! 0
                        <*> v .! 1
                        <*> (read <$> v .! 2) -- Convert string to Category
                        <*> v .! 3
        | otherwise = fail "Invalid number of fields in CSV record"



-- File to store transactions
transactionsFile :: FilePath
transactionsFile = "transactions.txt"

-- Main Program
main :: IO ()
main = do
    transactions <- loadTransactions
    let nextId = if null transactions then 1 else (maximum (map transactionId transactions) + 1)
    menu transactions nextId

-- Menu System
menu :: TransactionList -> Int -> IO ()
menu transactions nextId = do
    putStrLn "\n=== Personal Budget Management System ==="
    putStrLn "1. Add a Transaction"
    putStrLn "2. Update a Transaction"
    putStrLn "3. Remove a Transaction"
    putStrLn "4. View Transactions"
    putStrLn "5. Calculate Monthly Total"
    putStrLn "6. Generate Summary Report"
    putStrLn "7. Exit"
    putStr "Enter your choice: "
    hFlush stdout
    choice <- getLine
    case choice of
        "1" -> addTransaction transactions nextId
        "2" -> updateTransaction transactions nextId
        "3" -> removeTransaction transactions nextId
        "4" -> viewTransactions transactions nextId
        "5" -> calculateMonthlyTotal transactions nextId
        "6" -> generateSummaryReport transactions nextId
        "7" -> saveAndExit transactions
        _   -> do
            putStrLn "Invalid choice. Please try again."
            menu transactions nextId

type TransactionList = [Transaction]

-- 1. Add a Transaction
addTransaction :: TransactionList -> Int -> IO ()
addTransaction transactions nextId = do
    putStrLn "\n--- Add a Transaction ---"
    putStr "Enter the date (YYYY-MM-DD): "
    hFlush stdout
    dateInput <- getLine
    putStrLn "Enter the category (Food, Rent, Entertainment, Income, Other): "
    categoryInput <- getLine
    putStr "Enter the amount: "
    hFlush stdout
    amountInput <- getLine
    case (reads categoryInput :: [(Category, String)], reads amountInput :: [(Float, String)]) of
        ([(cat, "")], [(amt, "")]) -> do
            let transaction = Transaction nextId dateInput cat amt
            putStrLn $ "Transaction added: " ++ show transaction
            menu (transactions ++ [transaction]) (nextId + 1)
        _ -> do
            putStrLn "Invalid input. Please try again."
            addTransaction transactions nextId


-- 2. Update a Transaction
updateTransaction :: TransactionList -> Int -> IO ()
updateTransaction transactions nextId = do
    putStrLn "\n--- Update a Transaction ---"
    putStr "Enter the Transaction ID to update: "
    hFlush stdout
    idInput <- getLine
    case reads idInput :: [(Int, String)] of
        [(tid, "")] ->
            case filter (\t -> transactionId t == tid) transactions of
                [transaction] -> do
                    putStr "Enter the new date (YYYY-MM-DD): "
                    hFlush stdout
                    newDate <- getLine
                    putStrLn "Enter the new category (Food, Rent, Entertainment, Income, Other): "
                    newCategory <- getLine
                    putStr "Enter the new amount: "
                    hFlush stdout
                    newAmount <- getLine
                    case (reads newCategory :: [(Category, String)], reads newAmount :: [(Float, String)]) of
                        ([(cat, "")], [(amt, "")]) -> do
                            let updatedTransaction = transaction {date = newDate, category = cat, amount = amt}
                            let updatedList = map (\t -> if transactionId t == tid then updatedTransaction else t) transactions
                            putStrLn $ "Transaction updated: " ++ show updatedTransaction
                            menu updatedList nextId
                        _ -> do
                            putStrLn "Invalid input. Please try again."
                            updateTransaction transactions nextId
                [] -> do
                    putStrLn "Transaction ID not found. Please try again."
                    menu transactions nextId
        _ -> do
            putStrLn "Invalid ID. Please try again."
            menu transactions nextId

-- 3. Remove a Transaction
removeTransaction :: TransactionList -> Int -> IO ()
removeTransaction transactions nextId = do
    putStrLn "\n--- Remove a Transaction ---"
    putStr "Enter the Transaction ID to remove: "
    hFlush stdout
    idInput <- getLine
    case reads idInput :: [(Int, String)] of
        [(tid, "")] ->
            case filter (\t -> transactionId t == tid) transactions of
                [transaction] -> do
                    let updatedList = deleteBy (\a b -> transactionId a == transactionId b) transaction transactions
                    putStrLn $ "Transaction removed: " ++ show transaction
                    menu updatedList nextId
                [] -> do
                    putStrLn "Transaction ID not found. Please try again."
                    menu transactions nextId
        _ -> do
            putStrLn "Invalid ID. Please try again."
            menu transactions nextId

-- 4. View Transactions
viewTransactions :: TransactionList -> Int -> IO ()
viewTransactions transactions nextId = do
    putStrLn "\n--- View Transactions ---"
    if null transactions
        then putStrLn "No transactions found."
        else mapM_ print transactions
    menu transactions nextId


-- 5. Calculate Monthly Total
calculateMonthlyTotal :: TransactionList -> Int -> IO ()
calculateMonthlyTotal transactions nextId = do
    putStrLn "\n--- Calculate Monthly Total ---"
    putStr "Enter the month (YYYY-MM): "
    hFlush stdout
    month <- getLine
    let monthlyTransactions = filter (\t -> take 7 (date t) == month) transactions
    let totalIncome = sum [amount t | t <- monthlyTransactions, category t == Income]
    let totalExpenses = sum [amount t | t <- monthlyTransactions, category t /= Income]
    putStrLn $ "Total Income: " ++ show totalIncome
    putStrLn $ "Total Expenses: " ++ show totalExpenses
    putStrLn $ "Balance: " ++ show (totalIncome - totalExpenses)
    menu transactions nextId

-- 6. Generate Summary Report
generateSummaryReport :: TransactionList -> Int -> IO ()
generateSummaryReport transactions nextId = do
    putStrLn "\n--- Generate Summary Report ---"
    let totalIncome = sum [amount t | t <- transactions, category t == Income]
    let totalExpenses = sum [amount t | t <- transactions, category t /= Income]
    let balance = totalIncome - totalExpenses
    let categorizedTotals = map (\ts -> (category (head ts), sum (map amount ts))) $
                            groupBy ((==) `on` category) $
                            sortOn category transactions
    putStrLn $ "Total Income: " ++ show totalIncome
    putStrLn $ "Total Expenses: " ++ show totalExpenses
    putStrLn $ "Balance: " ++ show balance
    putStrLn "\nCategorized Totals:"
    mapM_ (\(cat, total) -> putStrLn $ show cat ++ ": " ++ show total) categorizedTotals
    menu transactions nextId

-- Save Transactions to CSV
saveTransactions :: TransactionList -> IO ()
saveTransactions transactions = do
    let csvData = encode transactions -- Encodes the transactions list as CSV
    BL.writeFile transactionsFile csvData -- Writes to the file
    putStrLn "Transactions saved to CSV file."

-- Load Transactions from CSV
loadTransactions :: IO TransactionList
loadTransactions = do
    fileExists <- doesFileExist transactionsFile
    if fileExists
        then do
            csvData <- BL.readFile transactionsFile
            case decode NoHeader csvData of
                Right transactions -> return (V.toList transactions)
                Left err -> do
                    putStrLn $ "Error loading transactions: " ++ err
                    return []
        else return []

-- Save and Exit
saveAndExit :: TransactionList -> IO ()
saveAndExit transactions = do
    saveTransactions transactions
    putStrLn "Transactions saved. Goodbye!"
