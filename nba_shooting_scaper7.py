import pandas as pd
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.chrome.service import Service
from webdriver_manager.chrome import ChromeDriverManager
# --- NEW: Added NoSuchElementException ---
from selenium.common.exceptions import TimeoutException, NoSuchElementException 
from io import StringIO
import time

def scrape_nba_table(url, season, season_type):
    """
    Fetches NBA player shooting stats by loading the webpage in a
    browser, scraping the HTML table, and navigating through all
    pages.
    """
    
    print("Setting up browser driver...")
    try:
        # Automatically downloads and installs the correct ChromeDriver
        service = Service(ChromeDriverManager().install())
        
        # Set up Chrome options for "headless" mode (no browser window opens)
        options = webdriver.ChromeOptions()
        options.add_argument('--headless')
        options.add_argument('--no-sandbox')
        options.add_argument('--disable-dev-shm-usage')
        options.add_argument("user-agent=Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/107.0.0.0 Safari/537.36")
        
        driver = webdriver.Chrome(service=service, options=options)
        
    except Exception as e:
        print(f"Error setting up WebDriver: {e}")
        print("Please ensure Google Chrome is installed on your system.")
        return None

    print(f"Navigating to URL for {season} {season_type}...")
    driver.get(url)

    # We also need to accept the cookies, or they will block the view
    # Wait for the cookie accept button and click it
    cookie_button_selector = "button#onetrust-accept-btn-handler"
    try:
        WebDriverWait(driver, 10).until(
            EC.element_to_be_clickable((By.CSS_SELECTOR, cookie_button_selector))
        )
        driver.find_element(By.CSS_SELECTOR, cookie_button_selector).click()
        print("Accepted cookies.")
        # Wait a moment for the cookie banner to disappear
        time.sleep(2)
    except Exception as e:
        print(f"Could not find or click cookie button: {e}")
        # Continue anyway, it might not be present

    # --- START: Pagination Logic ---
    table_selector = "table.Crom_table__p1iZz"
    
    # --- UPDATED SELECTOR based on user feedback ---
    # Using the specific div class and button title
    next_button_selector = 'div.Pagination_buttons__YpLUe button[title="Next Page Button"]'
    
    print(f"Waiting for initial table ({table_selector}) to load...")
    
    try:
        # Wait for the *first* page to load
        WebDriverWait(driver, 30).until(
            EC.presence_of_element_located((By.CSS_SELECTOR, table_selector))
        )
        
        all_dfs = []
        page_num = 1
        
        # --- MODIFIED LOOP ---
        # We will loop 5 times, as specified by the user.
        while page_num <= 5:
            print(f"Scraping page {page_num}...")
            
            # Get the HTML of the *current* table
            table_element = driver.find_element(By.CSS_SELECTOR, table_selector)
            table_html = table_element.get_attribute('outerHTML')
            
            # Use pandas to read the HTML table string
            df_list = pd.read_html(StringIO(table_html))
            
            if not df_list:
                print(f"Error: Pandas could not parse table on page {page_num}.")
                # If page 1 fails, exit.
                if page_num == 1: return None 
                # If a later page fails, stop and return what we have.
                print("Returning data scraped so far.")
                break
                
            df = df_list[0]
            
            # Clean up multi-level columns (only print message on first page)
            if isinstance(df.columns, pd.MultiIndex):
                if page_num == 1:
                     print("Cleaning up multi-level column headers...")
                df.columns = [' '.join(col).strip() for col in df.columns.values]

            all_dfs.append(df)

            # --- Check if we need to click "Next Page" ---
            # If we've just scraped page 5, we are done.
            if page_num == 5:
                print("Scraped final page (5).")
                break
            
            # If not page 5, find and click the "Next" button.
            try:
                # 1. Find the 'Next Page' button using the new selector.
                next_button = driver.find_element(By.CSS_SELECTOR, next_button_selector)

                # 2. Get staleness reference *before* the click.
                first_row = driver.find_element(By.CSS_SELECTOR, 'table.Crom_table__p1iZz tbody tr:first-child')

                # 3. Scroll the button into view and click.
                print(f"Found active 'Next Page' button, scrolling and clicking to get to page {page_num + 1}...")
                driver.execute_script("arguments[0].scrollIntoView(true);", next_button)
                time.sleep(0.5) # Give scroll a moment
                driver.execute_script("arguments[0].click();", next_button)
                
                # 4. Wait for the old data (first_row) to go stale.
                print("Waiting for new page to load...")
                WebDriverWait(driver, 10).until(EC.staleness_of(first_row))
                
                # 5. Wait for the new table to be present (good practice).
                WebDriverWait(driver, 10).until(
                    EC.presence_of_element_located((By.CSS_SELECTOR, table_selector))
                )
                
                # 6. Increment page number
                page_num += 1
                time.sleep(1) # Small buffer for safety

            except (NoSuchElementException, TimeoutException):
                print(f"Error: Could not find or click 'Next Page' button on page {page_num}.")
                print("This might be because the page hasn't loaded or the selector is wrong.")
                print("Returning data scraped so far.")
                break # Exit the while loop
            except Exception as e:
                print(f"Error during pagination: {e}")
                print("Returning data scraped so far.")
                break # Stop on any unexpected error

        if not all_dfs:
            # This handles the case where the table was empty on page 1
            print("No data was scraped.")
            return None
            
        print(f"Scraped a total of {len(all_dfs)} pages.")
        print("Combining all pages...")
        # Concatenate all DataFrames from the list into one
        return pd.concat(all_dfs, ignore_index=True)

    except TimeoutException:
        print("Request timed out waiting for *initial* table to load.")
    except Exception as err:
        print(f"An unexpected error occurred: {err}")
        
    finally:
        # Always quit the browser
        print("Closing browser.")
        driver.quit()
        
    return None
    # --- END: Pagination Logic ---

def save_to_csv(dataframe, filename):
    """Saves a pandas DataFrame to a CSV file."""
    try:
        dataframe.to_csv(filename, index=False)
        print(f"Successfully saved data to {filename}")
    except Exception as e:
        print(f"Error saving file: {e}")

if __name__ == "__main__":
    # --- Configuration ---
    SEASON = "2024-25"
    SEASON_TYPE = "Playoffs"
    
    # URL provided by the user
    TARGET_URL = f"https://www.nba.com/stats/players/shooting?DistanceRange=By+Zone&Season={SEASON}&SeasonType={SEASON_TYPE}&PerMode=PerGame"
    
    OUTPUT_FILE = "nba_24-25_playoff_shooting_by_zone.csv"
    # ---------------------

    data_df = scrape_nba_table(TARGET_URL, SEASON, SEASON_TYPE)

    if data_df is not None and not data_df.empty:
        save_to_csv(data_df, OUTPUT_FILE)
    elif data_df is not None and data_df.empty:
        # This case should be less common now, as logic handles empty tables
        print(f"Warning: No data found for {SEASON} {SEASON_TYPE}.")
        print("This is expected if the playoffs have not started yet.")
    else:
        print("Scraping failed or no data was found.")
