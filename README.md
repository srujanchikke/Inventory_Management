# Inventory Management (COBOL)

A small menu-driven stock register written in COBOL. We use it to keep
track of what is on the shelf, what came in from suppliers and what was
sold over the counter.

## Building

You need GnuCOBOL 3.x.

```
cobc -x -o inventory inventory.cob
./inventory
```

On Windows the output is `inventory.exe`.

## What it does

1. List all products
2. Search a product by ID
3. Add a product (asks for GST % - only 0, 5 or 18 are accepted)
4. Update name / category / supplier / price / reorder level
5. Receive stock from a supplier
6. Record a sale - prints a receipt with GST
7. Process a return - refunds the price plus GST
8. Low-stock report (at or below reorder level)
9. Inventory valuation at selling price
10. Transaction history

Sales of 50 or more units of one item get a 5% bulk discount. GST is
worked out on the amount after the discount.

## Things to know

- Nothing is saved yet. Products and transactions live in memory and are
  gone when you exit. Five sample products are loaded at start-up.
- Limits are 200 products and 1000 transactions per session.
- GST rates on the sample items follow the slabs from September 2025.
  Check the HSN rate for anything new you add.

## License

All rights reserved. See `LICENSE`.
