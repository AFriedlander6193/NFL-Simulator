from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from webdriver_manager.chrome import ChromeDriverManager
from time import sleep
import os
import shutil
import time

import pandas as pd
import re

service = Service(ChromeDriverManager().install())
driver = webdriver.Chrome(service=service)

driver.get("https://auth.pff.com/")

email_field = driver.find_element(By.ID, "login-form_email") 
password_field = driver.find_element(By.ID, "login-form_password") 

email_field.send_keys("EMAIL")
password_field.send_keys("PASSWORD")

submit_button = driver.find_element(By.ID, "sign-in") 
submit_button.click()

link_text = "PFF.com"
link = driver.find_element(By.LINK_TEXT, link_text)
link.click()

login_text = "SIGN IN"
mylogin = driver.find_element(By.LINK_TEXT, login_text)
mylogin.click()



stats = [ "passing", "passing_depth", "time_in_pocket", "passing_allowed_pressure",
"rushing", "receiving", "receiving_depth", "receiving_concept", "receiving_scheme", "offense_blocking", "defense",
"defense_pass_rush", "defense_run", "defense_coverage", "defense_coverage_scheme"
]
years = list(range(2006, 2025))
weeks = list(range(1, 19))

### Temporary Values for Big Data Bowl
#stats = [
    #"passing","passing_depth", "passing_pressure", "passing_concept", "time_in_pocket", "passing_allowed_pressure", "rushing", 
#"receiving", "receiving_depth", "receiving_concept", "receiving_scheme", "offense_blocking", "defense",
#"defense_pass_rush", "defense_run", "defense_coverage", "defense_coverage_scheme"]

#years = [2022]
#weeks = list(range(1, 10))


MAX_RETRIES = 3  # Maximum number of retries for each download
for stat in stats:
    for year in years:
        for week in weeks:
            if stat == "rushing":
                newstat = "rushing_summary"
            elif stat == "passing":
                newstat = "passing_summary"
            elif stat == "receiving":
                newstat = "receiving_summary"
            elif stat == "defense":
                newstat = "defense_summary"
            elif stat == "defense_pass_rush":
                newstat = "pass_rush_summary"
            elif stat == "defense_run":
                newstat = "run_defense_summary"
            elif stat == "defense_coverage":
                newstat = "defense_coverage_summary"
            else:
                newstat = stat
            currentdir = "C:/Users/Aaron Friedlander/Desktop/All Football Data/PFF Stats/" + newstat + "/" + str(year)
            os.chdir(currentdir)
            filename = "Week " + str(week) + ".csv"
            baseurl = "https://premium.pff.com/nfl/positions/"
            linkstat = stat.replace("_", "-")
            current = baseurl + str(year) + "/SINGLE/" + linkstat + "?position=QB&week=" + str(week)

            retries = 0
            while retries < MAX_RETRIES:
                try:
                    driver.get(current)
                    sleep(5)
                    download_button = driver.find_element(By.XPATH, '/html/body/div[1]/div/div[2]/div/div[1]/button')
                    download_button.click()
                    sleep(5)
                    for c in range(1, 20):
                        testpath = "C:/Users/Aaron Friedlander/Downloads/" + newstat + " (" + str(c) + ").csv"
                        if os.path.exists(testpath):
                            pattern = "(" + str(c) + ").csv"
                            break

                    if newstat != "passing":
                        source_path = "C:/Users/Aaron Friedlander/Downloads/" + newstat +  " " + pattern
                        destination = currentdir
                        os.makedirs(destination, exist_ok=True)
                        dummything = newstat + " " + pattern
                        destination_path = os.path.join(destination, dummything)
                        shutil.move(source_path, destination_path)

                        moved_file_path = os.path.expanduser("C:/Users/Aaron Friedlander/Desktop/All Football Data/PFF Stats/" + str(newstat) + "/" + str(year) + "/" + newstat +  " (1).csv")
                        new_file_path = currentdir + "/" + filename
                        if os.path.exists(new_file_path):
                            print(f"File for {stat} {year} Week {week} already exists, skipping...")
                            break
                        os.rename(moved_file_path, new_file_path)
                    print(f"Successfully downloaded and moved {newstat} {year} Week {week}")
                    break
                except Exception as e:
                    retries += 1
                    print(f"Error with {stat} {year} Week {week}: {e}. Retrying {retries}/{MAX_RETRIES}...")
                    del_file = "C:/Users/Aaron Friedlander/Downloads/passing_summary " + pattern + ".crdownload"
                    if os.path.exists(del_file):
                        os.remove(del_file)
                        print(f"{del_file} deleted successfully")
                    time.sleep(5)
            sleep(1)








currentdir = "C:/Users/Aaron Friedlander/Desktop/All Football Data/PFF Stats/" + stat + "/" + str(year)
os.chdir(currentdir)
filename = "Week " + str(week) + ".csv"
baseurl = "https://premium.pff.com/nfl/positions/"
linkstat = stat.replace("_", "-")
current = baseurl + str(year) + "/SINGLE/" + linkstat + "?position=QB&week=" + str(week)
driver.get(current)
sleep(3)
download_button = driver.find_element(By.XPATH, '/html/body/div[1]/div/div[2]/div/div[1]/button')
download_button.click()
sleep(5)
if stat == "passing":
    source_path = "C:/Users/Aaron Friedlander/Downloads/passing_summary (1).csv"
    destination = currentdir
    os.makedirs(destination, exist_ok=True)
    destination_path = os.path.join(destination, "passing_summary (1).csv")
    shutil.move(source_path, destination_path)

    moved_file_path = os.path.expanduser("C:/Users/Aaron Friedlander/Desktop/All Football Data/PFF Stats/passing/" + str(year) + "/passing_summary (1).csv")
    new_file_path = currentdir + "/" + filename
    os.rename(moved_file_path, new_file_path)

if stat != "passing":
    if stat == "rushing":
        stat = "rushing_summary"
    source_path = "C:/Users/Aaron Friedlander/Downloads/" + stat +  " (1).csv"
    destination = currentdir
    os.makedirs(destination, exist_ok=True)
    dummything = stat + " (1).csv"
    destination_path = os.path.join(destination, dummything)
    shutil.move(source_path, destination_path)

    moved_file_path = os.path.expanduser("C:/Users/Aaron Friedlander/Desktop/All Football Data/PFF Stats/" + str(stat) + "/" + str(year) + "/" + stat +  " (1).csv")
    new_file_path = currentdir + "/" + filename
    os.rename(moved_file_path, new_file_path)
sleep(1)


os.rename(moved_file_path, new_file_path)