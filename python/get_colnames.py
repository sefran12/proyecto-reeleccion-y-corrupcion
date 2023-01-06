# Write a function that reads all the files in the directory "data" and
# reads with pandas all the csv and xlsx files, only the first row, and returns
# a dataframe with a column as the file name and another column with the column names as a list converted to a string

def get_colnames(MAX_FILES=-1):
    import os
    import pandas as pd
    import logging
    # set logging level
    logging.basicConfig(
        level=logging.INFO,
        filename='data/02_intermediate/get_colnames.log',
        filemode='w',
        format='%(asctime)s - %(levelname)s - %(message)s'
    )
    # get all files in the directory, recursively, as their full path
    files = [os.path.join(dp, f) for dp, dn, filenames in os.walk(
        'data/01_raw') for f in filenames]
    # If MAX_FILES is -1, read all files
    if MAX_FILES == -1:
        MAX_FILES = len(files)
    # create empty dataframe
    df = pd.DataFrame(columns=['file', 'colnames'])
    # loop through files
    for file in files[0:MAX_FILES]:
        logging.info("Reading file: " + file)
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
            elif file.endswith('.sav') or file.endswith('.SAV'):
                # read file with pandas
                df_temp = pd.read_spss(file)
            else:
                logging.warning('File is not csv or xlsx or sav')
                continue
            # add file name to dataframe
            df_temp['file'] = file
            # add column names to dataframe
            logging.info('Column names as read:' + str(df_temp.columns.tolist()))
            df_temp['colnames'] = str(df_temp.columns.tolist())
            # add dataframe to dataframe
            df = df.append(df_temp[['file', 'colnames']], ignore_index=True)
        except Exception as e:
            logging.warning('File could not be read')
            logging.warning(e)
    # reset index
    df = df.reset_index(drop=True)
    # return dataframe
    return df


if __name__ == '__main__':
    # save colnames to memory and print
    colnames = get_colnames(-1)
    print(colnames)
    # save colnames to csv file
    colnames.to_csv('data/02_intermediate/colnames.csv',
                    index=False, encoding='utf-8-sig')
    colnames.to_excel('data/02_intermediate/colnames.xlsx',
                      index=False, encoding='utf-8-sig')
