--INSERTAR EMPRESAS DE ENVÍO NUEVAS
CREATE PROCEDURE InsertShipper 
@IDShipper int, 
@CompanyName nvarchar(40),
@Phone nvarchar(24)
AS
BEGIN 
	INSERT INTO dbo.Shippers (ShipperID, CompanyName, Phone)
	VALUES (@IDShipper, @CompanyName, @Phone)

END

SELECT * FROM Shippers;
--Prueba proceso 1
EXEC InsertShipper 4, 'Expreco', '(504) 2224-9003'

