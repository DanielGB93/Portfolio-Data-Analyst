--Muestra las ventas de empleados que fueron contratados despu�s del 1 de enero de 1994
SELECT *
FROM Orders
WHERE EmployeeID IN
  (SELECT EmployeeID
  FROM Employees
  WHERE HireDate >'1994')

--Selecciona el nombre y apellido de los empleados que hayan realizado las 10 ventas m�s grandes registradas.
SELECT FirstName, LastName
FROM Employees
WHERE EmployeeID IN
(	SELECT EmployeeID
	FROM Orders
	WHERE OrderID IN
(
	SELECT sub.OrderID
	FROM (
		SELECT TOP 10 OrderID, SUM(TotalSell) AS Total
		FROM [Order Details]
		GROUP BY OrderID
		ORDER BY Total) sub))

 