SET GLOBAL sql_mode = 'ONLY_FULL_GROUP_BY';

use superstoredb;
show tables;
describe cust_dimen;
describe market_fact;
describe orders_dimen;
describe prod_dimen;
describe shipping_dimen;

# Task 1
# Market_fact. this doesn't have any primary key cust_id is Foreign key from cust_dimen table Ord_ID is the Foreign key from orders_dimen. 
# Prod_Id is foreign key from prod_dimen Ship_id is foreign key from shipping_dimen.
# cust_dimen, cust_id is the primary key. No foreign key. 
# orders_dimen, Ord_ID is the primary key. No foreign key. 
# prod_dimen Prod_Id is the primary key. No foreign key.
# shipping_dimen Ship_id is the primary key.


#Task 2
# A Find the total and the average sales (display total_sales and avg_sales) 
select sum(Sales) as total_sales, avg(Sales) as avg_sales from market_fact;

# B Display the number of customers in each region in decreasing order of 
#   no_of_customers. The result should  contain columns Region, no_of_customers
select Region, count(*) as no_of_customers from cust_dimen group by Region order by no_of_customers desc;

# C Find the region having maximum customers (display the region name and max (no_of_customers)
select Region, count(*) as no_of_customers from cust_dimen group by Region order by no_of_customers desc limit 1;

# D Find the number and id of products sold in decreasing order of products sold (display 
#   product id, no_of_products sold)
select Prod_id as ProductId, count(*) as no_of_products_sold from market_fact group by Prod_Id order by no_of_products_sold desc;

# E Find all the customers from Atlantic region who have ever purchased ‘TABLES’ and the number of 
#   tables purchased (display the customer name, no_of_tables purchased) 
select Customer_Name, Region, Prod_id, count(*) as no_of_tables from market_fact 
	inner join cust_dimen on cust_dimen.Cust_id = market_fact.Cust_id 
    where Prod_id = (select Prod_id from prod_dimen where Product_Sub_Category = "TABLES") && Region = "ATLANTIC"
    group by Customer_Name;
    

# Task 3
# A Display the product categories in descending order of profits (display the product 
#   category wise profits i.e. product_category, profits)?
select Product_Category, sum(Profit) from market_fact 
	inner join prod_dimen on prod_dimen.Prod_id = market_fact.Prod_id 
    group by Product_Category;
    
# B Display the product category, product sub-category and the profit within each sub-category in three columns. 
select Product_Category, Product_Sub_Category, sum(Profit) as profit from market_fact 
	inner join prod_dimen on prod_dimen.Prod_id = market_fact.Prod_id 
    group by Product_Sub_Category;
    
# C Where is the least profitable product subcategory shipped the most? For the least profitable product sub-category, 
#   display the region-wise no_of_shipments and the profit made in each region in decreasing order of profits (i.e. region, no_of_shipments, profit_in_each_region)
#   Note: You can hardcode the name of the least profitable product sub-category    
select cust_dimen.Region, sum(Profit) as profit, count(*) as no_of_shipments from market_fact 
	inner join cust_dimen on cust_dimen.Cust_id = market_fact.Cust_id 
    where market_fact.Prod_id = "Prod_11"
    group by cust_dimen.Region
    order by profit desc;