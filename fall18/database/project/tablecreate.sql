CREATE DATABASE project;
USE project;

CREATE TABLE IF NOT EXISTS brand
(
    brand_id    bigint(4)   not null,
    brand_name  text(100),
    brand_type  text(20),
    PRIMARY KEY (brand_id)
);

CREATE TABLE IF NOT EXISTS  customer
(
    cust_code       bigint(4)   not null,
    cust_fname      text(20),
    cust_lname      text(20),
    cust_street     text(70),
    cust_city       text(50),
    cust_state      text(2),
    cust_zip        text(5),
    cust_balance    decimal(16) not null,
    PRIMARY KEY (cust_code)
);

CREATE TABLE IF NOT EXISTS  department
(
    dept_num        bigint(4)   not null,
    dept_name       text(50),
    dept_mail_box   text(3),
    dept_phone      text(9),
    supv_emp_num    bigint(4)   not null,
    PRIMARY KEY (dept_num)
);

CREATE TABLE IF NOT EXISTS  employee
(
    emp_num         bigint(4)   not null,
    emp_fname       text(20),
    emp_lname       text(20),
    emp_email       text(25),
    emp_phone       text(20),
    emp_hiredate    datetime not null,
    emp_title       text(45),
    emp_comm        decimal(16) not null,
    dept_num        bigint(4)   not null,
    PRIMARY KEY (emp_num),
    INDEX   idx_dn(dept_num asc),
    INDEX   idx_en(emp_num asc)
);

CREATE TABLE IF NOT EXISTS  invoice
(
    inv_num     bigint(4)   not null,
    inv_date    datetime not null,
    cust_code   bigint(4)   not null,
    inv_total   decimal(16) not null,
    emp_num     bigint(4)   not null,
    PRIMARY KEY (inv_num),
    INDEX   idx_cc(cust_code asc),
    INDEX   idx_in(inv_num asc),
    INDEX   idx_en(emp_num asc)
);

CREATE TABLE IF NOT EXISTS  line
(
    inv_num     bigint(4)   not null,
    line_num    bigint(4)   not null,
    prod_sku    text(15),
    line_qty    bigint(8)   not null,
    line_price  decimal(16) not null
);

CREATE TABLE IF NOT EXISTS  product
(
    prod_sku        text(15),
    prod_descript   text(255),
    prod_type       text(255),
    prod_base       text(255),
    prod_category   text(255),
    prod_price      decimal(16) not null,
    prod_qoh        decimal(16) not null,
    prod_min        decimal(16) not null,
    brand_id        bigint(4)   not null
);

CREATE TABLE IF NOT EXISTS  salary_history
(
    emp_num     bigint(4)   not null,
    sal_from    datetime not null,
    sal_end     datetime,
    sal_amount  decimal(16) not null
);

CREATE TABLE IF NOT EXISTS  supplies
(
    prod_sku    text(15),
    vend_id     bigint(4)   not null
);

CREATE TABLE IF NOT EXISTS  vendor
(
    vend_id     bigint(4)   not null,
    vend_name   text(255),
    vend_street text(50),
    vend_city   text(50),
    vend_state  text(2),
    vend_zip    text(5),
    PRIMARY KEY (vend_id)
);




ALTER TABLE product
MODIFY COLUMN prod_sku varchar(255);
ALTER TABLE product
ADD PRIMARY KEY (prod_sku);
ALTER TABLE product
ADD FOREIGN KEY (brand_id) REFERENCES brand(brand_id);


ALTER TABLE department
ADD FOREIGN KEY (supv_emp_num) REFERENCES employee(emp_num);


ALTER TABLE employee
ADD FOREIGN KEY (dept_num) REFERENCES department(dept_num);


ALTER TABLE invoice
ADD FOREIGN KEY (cust_code) REFERENCES customer(cust_code),
ADD FOREIGN KEY (emp_num) REFERENCES employee(emp_num);


ALTER TABLE line
MODIFY COLUMN prod_sku varchar(255);
ALTER TABLE line
ADD PRIMARY KEY (prod_sku,inv_num);
ALTER TABLE line
ADD FOREIGN KEY (inv_num) REFERENCES invoice(inv_num);


ALTER TABLE salary_history
ADD PRIMARY KEY (emp_num,sal_from);
ALTER TABLE salary_history
ADD FOREIGN KEY (emp_num) REFERENCES employee(emp_num);


ALTER TABLE supplies
MODIFY COLUMN prod_sku varchar(255);
ALTER TABLE supplies
ADD PRIMARY KEY (prod_sku);
ALTER TABLE supplies
ADD FOREIGN KEY (vend_id) REFERENCES vendor(vend_id);
ALTER TABLE supplies
ADD FOREIGN KEY (prod_sku) REFERENCES product(prod_sku);



select count(*) from brand;
select count(*) from customer;
select count(*) from department;
select count(*) from employee;
select count(*) from invoice;
select count(*) from line;
select count(*) from product;
select count(*) from salary_history;
select count(*) from supplies;
select count(*) from vendor;