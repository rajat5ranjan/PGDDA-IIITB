## TASK 1 : Understanding the data in hand ##
   ## TABLE cust_dimen ##
   ## COLUMN Cust_id  -> VARCHAR(25) -> Primary Key
   #----------------------------------------------------------------------#
   ## TABLE prod_dimen ##
   ## COLUMN Prod_id  -> VARCHAR(25) -> Primary Key
   #----------------------------------------------------------------------#
   ## TABLE order_dimen ##
   ## COLUMN Ord_id   -> VARCHAR(25) -> Primary Key
   ## COLUMN Order_ID -> VARCHAR(25) -> INDEXED
   #----------------------------------------------------------------------#
   ## TABLE shipping_dimen ##
   ## COLUMN Ship_id  -> VARCHAR(25) -> PRIMARY KEY
   ## COLUMN Order_id -> VARCHAR(25) -> FOREIGN KEY REFER TO order_dimen.Order_ID
   #----------------------------------------------------------------------#
   ## TABLE market_fact ##
   ## COLUMN Ord_id   -> VARCHAR(25) -> FOREIGN KEY REFER TO orders_dimen.Ord_id
   ## COLUMN Prod_id  -> VARCHAR(25) -> FOREIGN KEY REFER TO prod_dimen.Prod_id
   ## COLUMN Ship_id  -> VARCHAR(25) -> FOREIGN KEY REFER TO -> shipping_dimen.Ship_id
   ## COLUMN Cust_id  -> VARCHAR(25) -> FOREIGN KEY REFER TO cust_dimen.Cust_id

## Task 2 : Basic Analysis ##
   ## Q1) Find the total and the average sales (display total_sales and avg_sales)
   select round(sum(Sales),2) as total_sales, round(avg(Sales),2) as avg_sales from market_fact;
   
   ## Q2) Display the number of customers in each region in decreasing order of
   ##     no_of_customers. The result should be a table with columns Region,
   ##     no_of_customers
   select Region,count(*) as no_of_customer from cust_dimen group by Region order by no_of_customer DESC;
   
   ## Q3) Find the region having maximum customers (display the region name and
   ##     max(no_of_customers)
   select Region,count(*) as 'max(no_of_customer)' from cust_dimen group by Region 
   having `max(no_of_customer)` = 
   (select max(t1.no_of_customer) from (select Region,count(*) as 'no_of_customer' from cust_dimen group by Region) t1);
   
   ## Q4) Find the number and id of products sold in decreasing order of products sold (display
   ##     product id, no_of_products sold)
   select Prod_id as 'product id',sum(Order_Quantity) as 'no_of_products sold' from market_fact group by Prod_id order by `no_of_products sold` DESC;
   
   ## Q5) Find all the customers from Atlantic region who have ever purchased ‘TABLES’ and
   ##     the number of tables purchased (display the customer name, no_of_tables
   ##     purchased)
   select t2.Customer_Name as 'customer name',sum(t1.Order_Quantity) as 'no_of_tables' from market_fact t1 right join 
   (select Customer_Name,Cust_id from cust_dimen where Region = "Atlantic") t2 on t1.Cust_id = t2.Cust_id
   where t1.Prod_id = (select Prod_id from prod_dimen where Product_Sub_Category like '%TABLES%') group by t1.Cust_id;
   
## Task 3 : Advanced Analysis
   ## Q1) Display the product categories in descending order of profits (display the product
   ##     category wise profits i.e. product_category, profits)?
   select t1.Product_Category as 'product_category',sum(t2.profits) as'profits' from prod_dimen t1 left join 
   (select Prod_id,round(sum(Profit),2) as 'profits' from market_fact group by Prod_id) t2 on t1.Prod_id = t2.Prod_id 
   group by t1.Product_Category order by `profits` DESC;
   
   ## Q2) Display the product category, product sub-category and the profit within each
   ##     subcategory in three columns.
   select t1.Product_Category as 'product_category',t1.Product_Sub_Category as 'product_sub_category', sum(t2.profits) as'profits' from prod_dimen t1 left join 
   (select Prod_id,round(sum(Profit),2) as 'profits' from market_fact group by Prod_id) t2 on t1.Prod_id = t2.Prod_id 
   group by t1.Product_Category,t1.Product_Sub_Category;

   
   ## Q3) Where is the least profitable product subcategory shipped the most? For the least
   ##     profitable product sub-category, display the region-wise no_of_shipments and the
   ##     profit made in each region in decreasing order of profits (i.e. region,
   ##     no_of_shipments, profit_in_each_region)
   ##     o Note: You can hardcode the name of the least profitable product subcategory
   
   ## Note : Least profitable product_sub_cateogy is TABLES (Based on analysis from previous query
   ##      : by just adding order by profit to query for Q2) above
   select t2.Region as 'region',count(t1.Ship_id) as 'no_of_shipment',round(sum(t1.Profit),2) as 'profit_in_each_region' from market_fact t1 left join 
   cust_dimen t2 on t1.Cust_id = t2.Cust_id 
   where t1.Prod_id = 
   (select Prod_id from prod_dimen where Product_Sub_Category like '%TABLE%') 
   group by t2.Region order by `profit_in_each_region` DESC;
   
   