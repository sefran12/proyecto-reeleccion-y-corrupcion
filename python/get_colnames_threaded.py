import threading

def get_colnames(files):
    import pandas as pd
    # create empty dataframe
    df = pd.DataFrame(columns=['file', 'colnames'])
    # loop through files
    for file in files:
        print("Reading file: " + file)
        row_temp = pd.DataFrame()
        try:
        # check if file is csv or xlsx
            if file.endswith('.csv'):        
                # read file with pandas
                df_temp = pd.read_csv(file, nrows=1)
            elif file.endswith('.xlsx'):
                # read file with pandas, if name contains "CONOSCE", read the second row as header}
                if 'CONOSCE' in file:
                    df_temp = pd.read_excel(file, nrows=1, skiprows=1)
                else:
                    df_temp = pd.read_excel(file, nrows=1)
            elif file.endswith('.sav'):
                # read file with pandas
                df_temp = pd.read_spss(file, nrows=1)
            else:
                print('File is not csv or xlsx or sav')
                continue
            # add file name to dataframe
            row_temp['file'] = file
            # add column names to dataframe
            print(df_temp.columns.tolist())
            row_temp['colnames'] = str(df_temp.columns.tolist())
            print(row_temp)
            # add dataframe to dataframe
            df = df.append(row_temp, ignore_index=True)
        except Exception as e:
            print('File could not be read')
            print(e)
    # reset index
    df = df.reset_index(drop=True)
    # return dataframe
    return df

def main():
    import os
    # get all files in the directory, recursively, as their full path
    files = [os.path.join(dp, f) for dp, dn, filenames in os.walk('data/01_raw') for f in filenames]
    # split list of files into smaller lists for each thread
    split_files = [files[i:i+5] for i in range(0, len(files), 5)]
    threads = []
    for file_list in split_files:
        # create a new thread for each file list
        thread = threading.Thread(target=get_colnames, args=(file_list,))
        thread.start()
        threads.append(thread)
    # wait for all threads to finish
    for thread in threads:
        thread.join()
    # concatenate results from each thread into a single dataframe
    colnames = pd.concat([thread.df for thread in threads], ignore_index=True)
    return colnames

if __name__ == '__main__':
    import pandas as pd
    # save colnames to memory and print
    colnames = main()
    print(colnames)
    # save colnames to csv file
    colnames.to_csv('data/02_intermediate/colnames.csv',
                    index=False, encoding='utf-8-sig')
    colnames.to_excel('data/02_intermediate/colnames.xlsx',
                      index=False, encoding='utf-8-sig')
