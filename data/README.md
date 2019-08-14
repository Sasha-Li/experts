# Variables code sheet

|Study         | Variable      | Values   | Meaning  
|--------------| ------------- |:--------:|:--------|
|Breast cancer | X1            | 1 - 5349 | row number
|              | doctor        | 1 - 119  | doctor ID
|              | picture       | 3 - 181  | picture ID
|              | cancer        | 0        | 0 = no cancer
|              | assess1       | 0/1      | first diagnosis: 0 = no cancer; 1 = cancer
|              | correct1      | 0/1      | accuracy of first assessment: 0 = wrong; 1 = correct
|              | conf1         | 1 - 5    | confidence in first diagnosis
|              | viewtime1     | 10 - 420 | viewtime in seconds for first assessment
|              | totaltime1    | 10 - 429 | view- and decisiontime in seconds for first assessment
|              | assess2       | 0/1      | second diagnosis: 0 = no cancer; 1 = cancer
|              | correct2      | 0/1      | accuracy of second assessment: 0 = wrong; 1 = correct
|              | conf2         | 1 - 5    | confidence in second diagnosis
|              | viewtime2     | ---      | viewtime in seconds for second assessment
|              | totaltime2    | ---      | view- and decisiontime in seconds for second assessment
|              | domain        | breast   | Study ID
|Back pain     | doctor        | 1 - 13   | doctor ID
|              | picture       | 1 - 300  | picture ID
|              | abnormality   | 0/1      | 0 = normal; 1 = abnormal
|              | assess1       | 0/1      | first assessment: 0 = normal; 1 = abnormal
|              | correct1      | 0/1      | accuracy of first assessment: 0 = wrong; 1 = correct
|              | confidence1   | 1 - 2    | confidence in first assessment
|              | assess2       | 0/1      | second assessment: 0 = normal; 1 = abnormal
|              | correct2      | 0/1      | accuracy of second assessment: 0 = wrong; 1 = correct
|              | confidence2   | 1 - 2    | confidence in second assessment
|              | domain        | back     | Study ID

