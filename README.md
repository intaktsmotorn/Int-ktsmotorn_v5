# Project Overview

I am developing an **internal analytics and billing management system** in **R using Shiny**.

The system acts as a lightweight operational database stored in **Excel** and provides a **UI for managing consultants, customers, assignments, tasks, time reporting, and financial calculations**.

The goal is to **replace Excel workflows with a structured application while still using Excel as the storage backend**.

---

# Tech Stack

**Language**
- R

**Framework**
- Shiny

**UI Tables**
- rhandsontable

**Data Storage**
- Excel (`.xlsx`)

**Packages Used**

- shiny
- dplyr
- readxl
- writexl
- lubridate
- janitor
- stringr
- rhandsontable

---

# Architecture

The application is a **single-file Shiny app (`app.R`)** that loads an **Excel workbook containing multiple sheets** that act as relational tables.

---

# Main Data Tables

## FaktureringInformation

| Column |
|------|
| faktura_mottagare_id |
| mottagare_typ |
| maklare_id |
| customer_id |
| kontakt_fornamn |
| kontakt_efternamn |
| email |
| avtal_nummer |
| referens |
| verksamhet |
| kommentar |

---

## Maklare

| Column |
|------|
| maklare_id |
| maklare_namn |
| kontakt_fornamn |
| kontakt_efternamn |
| email |
| kommentar |
| created_at |

---

## Kunder

| Column |
|------|
| customer_id |
| customer_namn |
| kontakt_fornamn |
| kontakt_efternamn |
| email |
| created_at |

---

## Konsulter

| Column |
|------|
| consultant_id |
| fornamn |
| efternamn |
| type |
| grundlon |
| bonus_grund |
| group_bonus |
| sales_bonus |
| startdatum |
| slutdatum |

---

## Uppdrag

| Column |
|------|
| uppdrag_id |
| uppdrag_name |
| beskrivning |
| startdatum |
| slutdatum |
| customer_id |
| faktura_mottagare_id |

---

## Uppgift

| Column |
|------|
| uppgift_id |
| uppgift_name |
| startdatum |
| slutdatum |
| timpris |
| created_at |
| consultant_id |
| uppdrag_id |
| customer_id |

---

## Tidrapportering

| Column |
|------|
| tidrapport_id |
| timmar |
| startdatum |
| slutdatum |
| created_at |
| uppgift_id |
| consultant_id |
| uppdrag_id |
| customer_id |

---

# History Tables

The system automatically logs changes into history tables.

## TimprisHistory

| Column |
|------|
| timpris_id |
| consultant_id |
| uppdrag_id |
| uppgift_id |
| timpris |
| created_at |
| kommentar |

---

## GrundlonHistory

| Column |
|------|
| lon_id |
| consultant_id |
| grundlon |
| created_at |
| kommentar |

---

## BonusHistory

| Column |
|------|
| bonus_id |
| consultant_id |
| bonus_grund |
| created_at |
| kommentar |

---

## GroupBonusHistory

| Column |
|------|
| group_bonus_id |
| consultant_id |
| group_bonus |
| created_at |
| kommentar |

---

## SalesBonusHistory

| Column |
|------|
| sales_bonus_id |
| consultant_id |
| customer_id |
| sales_bonus |
| created_at |
| kommentar |

---

## ArbetstimmarGrund

| Column |
|------|
| date |
| arbetstimmar |

---

# History Tracking

The system automatically logs changes into the following tables:

- GrundlonHistory  
- BonusHistory  
- GroupBonusHistory  
- SalesBonusHistory  
- TimprisHistory  

These tables track when values change and allow **historical analysis**.

---

# Key Functionality

## Consultant Management

- Add new consultants
- Edit consultants via **rhandsontable**
- Automatically log history when salary or bonus values change

---

## Customer and Broker Management

- Add customers
- Add brokers
- Link invoice recipients

---

## Assignments and Tasks

- Assignments belong to customers
- Tasks belong to assignments and consultants

---

## Time Reporting

Users report hours per month.

Features:

- Month selections automatically generate date ranges
- Each time entry is linked to:
  - task
  - consultant
  - customer

---

# Reporting Module

The application includes a **monthly revenue report**.

The report calculates:

- hours × hourly rate
- consultant totals
- bonus calculation above revenue threshold
- **debiteringsgrad (utilization rate)**

---

# Important Design Rules

- **Excel is the persistent storage**
- **Shiny reactive values hold the working state**
- Data is written to disk **only when the user presses save**
- History tables must log changes when values are modified
- Existing functionality must **not break** when adding new features

---

# Current Development Goal

Continue developing and improving the application in a **structured and maintainable way**.

Typical tasks include:

- Refactoring the code for clarity and maintainability
- Improving data validation
- Improving history tracking
- Adding new financial logic
- Improving Shiny performance
- Extending reporting features

---

# Instructions for Assistance

When suggesting code:

- Do **not break existing functionality**
- Modify **only necessary parts of the code**
- Clearly explain **where code should be placed**
- Follow **tidyverse-style R coding conventions**
- Keep **Shiny reactive logic stable**

Assume this is a **production-like internal tool** and changes should be **safe and minimal**.