¬®
÷Í
D
AddV2
x"T
y"T
z"T"
Ttype:
2	
^
AssignVariableOp
resource
value"dtype"
dtypetype"
validate_shapebool( 
~
BiasAdd

value"T	
bias"T
output"T" 
Ttype:
2	"-
data_formatstringNHWC:
NHWCNCHW
8
Const
output"dtype"
valuetensor"
dtypetype
,
Exp
x"T
y"T"
Ttype:

2
.
Identity

input"T
output"T"	
Ttype
q
MatMul
a"T
b"T
product"T"
transpose_abool( "
transpose_bbool( "
Ttype:

2	
e
MergeV2Checkpoints
checkpoint_prefixes
destination_prefix"
delete_old_dirsbool(
?
Mul
x"T
y"T
z"T"
Ttype:
2	

NoOp
M
Pack
values"T*N
output"T"
Nint(0"	
Ttype"
axisint 
C
Placeholder
output"dtype"
dtypetype"
shapeshape:

RandomStandardNormal

shape"T
output"dtype"
seedint "
seed2int "
dtypetype:
2"
Ttype:
2	
@
ReadVariableOp
resource
value"dtype"
dtypetype
o
	RestoreV2

prefix
tensor_names
shape_and_slices
tensors2dtypes"
dtypes
list(type)(0
l
SaveV2

prefix
tensor_names
shape_and_slices
tensors2dtypes"
dtypes
list(type)(0
?
Select
	condition

t"T
e"T
output"T"	
Ttype
P
Shape

input"T
output"out_type"	
Ttype"
out_typetype0:
2	
H
ShardedFilename
basename	
shard

num_shards
filename
9
Softmax
logits"T
softmax"T"
Ttype:
2
Á
StatefulPartitionedCall
args2Tin
output2Tout"
Tin
list(type)("
Tout
list(type)("	
ffunc"
configstring "
config_protostring "
executor_typestring ¨
@
StaticRegexFullMatch	
input

output
"
patternstring
ö
StridedSlice

input"T
begin"Index
end"Index
strides"Index
output"T"	
Ttype"
Indextype:
2	"

begin_maskint "
end_maskint "
ellipsis_maskint "
new_axis_maskint "
shrink_axis_maskint 
N

StringJoin
inputs*N

output"
Nint(0"
	separatorstring 
-
Tanh
x"T
y"T"
Ttype:

2

VarHandleOp
resource"
	containerstring "
shared_namestring "
dtypetype"
shapeshape"#
allowed_deviceslist(string)
 "serve*2.8.02v2.8.0-0-g3f878cff5b68þð


Encoding_layer_1/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape:
ý*(
shared_nameEncoding_layer_1/kernel

+Encoding_layer_1/kernel/Read/ReadVariableOpReadVariableOpEncoding_layer_1/kernel* 
_output_shapes
:
ý*
dtype0

Encoding_layer_1/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape:*&
shared_nameEncoding_layer_1/bias
|
)Encoding_layer_1/bias/Read/ReadVariableOpReadVariableOpEncoding_layer_1/bias*
_output_shapes	
:*
dtype0

Encoding_layer_2/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape:
*(
shared_nameEncoding_layer_2/kernel

+Encoding_layer_2/kernel/Read/ReadVariableOpReadVariableOpEncoding_layer_2/kernel* 
_output_shapes
:
*
dtype0

Encoding_layer_2/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape:*&
shared_nameEncoding_layer_2/bias
|
)Encoding_layer_2/bias/Read/ReadVariableOpReadVariableOpEncoding_layer_2/bias*
_output_shapes	
:*
dtype0

Encoding_layer_3/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape:	@*(
shared_nameEncoding_layer_3/kernel

+Encoding_layer_3/kernel/Read/ReadVariableOpReadVariableOpEncoding_layer_3/kernel*
_output_shapes
:	@*
dtype0

Encoding_layer_3/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape:@*&
shared_nameEncoding_layer_3/bias
{
)Encoding_layer_3/bias/Read/ReadVariableOpReadVariableOpEncoding_layer_3/bias*
_output_shapes
:@*
dtype0

Encoding_layer_4/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape
:@ *(
shared_nameEncoding_layer_4/kernel

+Encoding_layer_4/kernel/Read/ReadVariableOpReadVariableOpEncoding_layer_4/kernel*
_output_shapes

:@ *
dtype0

Encoding_layer_4/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape: *&
shared_nameEncoding_layer_4/bias
{
)Encoding_layer_4/bias/Read/ReadVariableOpReadVariableOpEncoding_layer_4/bias*
_output_shapes
: *
dtype0
v
z_mean/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape
: *
shared_namez_mean/kernel
o
!z_mean/kernel/Read/ReadVariableOpReadVariableOpz_mean/kernel*
_output_shapes

: *
dtype0
n
z_mean/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape:*
shared_namez_mean/bias
g
z_mean/bias/Read/ReadVariableOpReadVariableOpz_mean/bias*
_output_shapes
:*
dtype0

z_log_sigma/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape
: *#
shared_namez_log_sigma/kernel
y
&z_log_sigma/kernel/Read/ReadVariableOpReadVariableOpz_log_sigma/kernel*
_output_shapes

: *
dtype0
x
z_log_sigma/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape:*!
shared_namez_log_sigma/bias
q
$z_log_sigma/bias/Read/ReadVariableOpReadVariableOpz_log_sigma/bias*
_output_shapes
:*
dtype0

classification_layer/kernelVarHandleOp*
_output_shapes
: *
dtype0*
shape
: *,
shared_nameclassification_layer/kernel

/classification_layer/kernel/Read/ReadVariableOpReadVariableOpclassification_layer/kernel*
_output_shapes

: *
dtype0

classification_layer/biasVarHandleOp*
_output_shapes
: *
dtype0*
shape:**
shared_nameclassification_layer/bias

-classification_layer/bias/Read/ReadVariableOpReadVariableOpclassification_layer/bias*
_output_shapes
:*
dtype0
`
beta_1VarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_namebeta_1
Y
beta_1/Read/ReadVariableOpReadVariableOpbeta_1*
_output_shapes
: *
dtype0
`
beta_2VarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_namebeta_2
Y
beta_2/Read/ReadVariableOpReadVariableOpbeta_2*
_output_shapes
: *
dtype0
^
decayVarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_namedecay
W
decay/Read/ReadVariableOpReadVariableOpdecay*
_output_shapes
: *
dtype0
n
learning_rateVarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_namelearning_rate
g
!learning_rate/Read/ReadVariableOpReadVariableOplearning_rate*
_output_shapes
: *
dtype0
f
	Adam/iterVarHandleOp*
_output_shapes
: *
dtype0	*
shape: *
shared_name	Adam/iter
_
Adam/iter/Read/ReadVariableOpReadVariableOp	Adam/iter*
_output_shapes
: *
dtype0	
^
totalVarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_nametotal
W
total/Read/ReadVariableOpReadVariableOptotal*
_output_shapes
: *
dtype0
^
countVarHandleOp*
_output_shapes
: *
dtype0*
shape: *
shared_namecount
W
count/Read/ReadVariableOpReadVariableOpcount*
_output_shapes
: *
dtype0

Adam/Encoding_layer_1/kernel/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:
ý*/
shared_name Adam/Encoding_layer_1/kernel/m

2Adam/Encoding_layer_1/kernel/m/Read/ReadVariableOpReadVariableOpAdam/Encoding_layer_1/kernel/m* 
_output_shapes
:
ý*
dtype0

Adam/Encoding_layer_1/bias/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:*-
shared_nameAdam/Encoding_layer_1/bias/m

0Adam/Encoding_layer_1/bias/m/Read/ReadVariableOpReadVariableOpAdam/Encoding_layer_1/bias/m*
_output_shapes	
:*
dtype0

Adam/Encoding_layer_2/kernel/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:
*/
shared_name Adam/Encoding_layer_2/kernel/m

2Adam/Encoding_layer_2/kernel/m/Read/ReadVariableOpReadVariableOpAdam/Encoding_layer_2/kernel/m* 
_output_shapes
:
*
dtype0

Adam/Encoding_layer_2/bias/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:*-
shared_nameAdam/Encoding_layer_2/bias/m

0Adam/Encoding_layer_2/bias/m/Read/ReadVariableOpReadVariableOpAdam/Encoding_layer_2/bias/m*
_output_shapes	
:*
dtype0

Adam/Encoding_layer_3/kernel/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:	@*/
shared_name Adam/Encoding_layer_3/kernel/m

2Adam/Encoding_layer_3/kernel/m/Read/ReadVariableOpReadVariableOpAdam/Encoding_layer_3/kernel/m*
_output_shapes
:	@*
dtype0

Adam/Encoding_layer_3/bias/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:@*-
shared_nameAdam/Encoding_layer_3/bias/m

0Adam/Encoding_layer_3/bias/m/Read/ReadVariableOpReadVariableOpAdam/Encoding_layer_3/bias/m*
_output_shapes
:@*
dtype0

Adam/Encoding_layer_4/kernel/mVarHandleOp*
_output_shapes
: *
dtype0*
shape
:@ */
shared_name Adam/Encoding_layer_4/kernel/m

2Adam/Encoding_layer_4/kernel/m/Read/ReadVariableOpReadVariableOpAdam/Encoding_layer_4/kernel/m*
_output_shapes

:@ *
dtype0

Adam/Encoding_layer_4/bias/mVarHandleOp*
_output_shapes
: *
dtype0*
shape: *-
shared_nameAdam/Encoding_layer_4/bias/m

0Adam/Encoding_layer_4/bias/m/Read/ReadVariableOpReadVariableOpAdam/Encoding_layer_4/bias/m*
_output_shapes
: *
dtype0

Adam/z_mean/kernel/mVarHandleOp*
_output_shapes
: *
dtype0*
shape
: *%
shared_nameAdam/z_mean/kernel/m
}
(Adam/z_mean/kernel/m/Read/ReadVariableOpReadVariableOpAdam/z_mean/kernel/m*
_output_shapes

: *
dtype0
|
Adam/z_mean/bias/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:*#
shared_nameAdam/z_mean/bias/m
u
&Adam/z_mean/bias/m/Read/ReadVariableOpReadVariableOpAdam/z_mean/bias/m*
_output_shapes
:*
dtype0

Adam/z_log_sigma/kernel/mVarHandleOp*
_output_shapes
: *
dtype0*
shape
: **
shared_nameAdam/z_log_sigma/kernel/m

-Adam/z_log_sigma/kernel/m/Read/ReadVariableOpReadVariableOpAdam/z_log_sigma/kernel/m*
_output_shapes

: *
dtype0

Adam/z_log_sigma/bias/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:*(
shared_nameAdam/z_log_sigma/bias/m

+Adam/z_log_sigma/bias/m/Read/ReadVariableOpReadVariableOpAdam/z_log_sigma/bias/m*
_output_shapes
:*
dtype0
 
"Adam/classification_layer/kernel/mVarHandleOp*
_output_shapes
: *
dtype0*
shape
: *3
shared_name$"Adam/classification_layer/kernel/m

6Adam/classification_layer/kernel/m/Read/ReadVariableOpReadVariableOp"Adam/classification_layer/kernel/m*
_output_shapes

: *
dtype0

 Adam/classification_layer/bias/mVarHandleOp*
_output_shapes
: *
dtype0*
shape:*1
shared_name" Adam/classification_layer/bias/m

4Adam/classification_layer/bias/m/Read/ReadVariableOpReadVariableOp Adam/classification_layer/bias/m*
_output_shapes
:*
dtype0

Adam/Encoding_layer_1/kernel/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:
ý*/
shared_name Adam/Encoding_layer_1/kernel/v

2Adam/Encoding_layer_1/kernel/v/Read/ReadVariableOpReadVariableOpAdam/Encoding_layer_1/kernel/v* 
_output_shapes
:
ý*
dtype0

Adam/Encoding_layer_1/bias/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:*-
shared_nameAdam/Encoding_layer_1/bias/v

0Adam/Encoding_layer_1/bias/v/Read/ReadVariableOpReadVariableOpAdam/Encoding_layer_1/bias/v*
_output_shapes	
:*
dtype0

Adam/Encoding_layer_2/kernel/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:
*/
shared_name Adam/Encoding_layer_2/kernel/v

2Adam/Encoding_layer_2/kernel/v/Read/ReadVariableOpReadVariableOpAdam/Encoding_layer_2/kernel/v* 
_output_shapes
:
*
dtype0

Adam/Encoding_layer_2/bias/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:*-
shared_nameAdam/Encoding_layer_2/bias/v

0Adam/Encoding_layer_2/bias/v/Read/ReadVariableOpReadVariableOpAdam/Encoding_layer_2/bias/v*
_output_shapes	
:*
dtype0

Adam/Encoding_layer_3/kernel/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:	@*/
shared_name Adam/Encoding_layer_3/kernel/v

2Adam/Encoding_layer_3/kernel/v/Read/ReadVariableOpReadVariableOpAdam/Encoding_layer_3/kernel/v*
_output_shapes
:	@*
dtype0

Adam/Encoding_layer_3/bias/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:@*-
shared_nameAdam/Encoding_layer_3/bias/v

0Adam/Encoding_layer_3/bias/v/Read/ReadVariableOpReadVariableOpAdam/Encoding_layer_3/bias/v*
_output_shapes
:@*
dtype0

Adam/Encoding_layer_4/kernel/vVarHandleOp*
_output_shapes
: *
dtype0*
shape
:@ */
shared_name Adam/Encoding_layer_4/kernel/v

2Adam/Encoding_layer_4/kernel/v/Read/ReadVariableOpReadVariableOpAdam/Encoding_layer_4/kernel/v*
_output_shapes

:@ *
dtype0

Adam/Encoding_layer_4/bias/vVarHandleOp*
_output_shapes
: *
dtype0*
shape: *-
shared_nameAdam/Encoding_layer_4/bias/v

0Adam/Encoding_layer_4/bias/v/Read/ReadVariableOpReadVariableOpAdam/Encoding_layer_4/bias/v*
_output_shapes
: *
dtype0

Adam/z_mean/kernel/vVarHandleOp*
_output_shapes
: *
dtype0*
shape
: *%
shared_nameAdam/z_mean/kernel/v
}
(Adam/z_mean/kernel/v/Read/ReadVariableOpReadVariableOpAdam/z_mean/kernel/v*
_output_shapes

: *
dtype0
|
Adam/z_mean/bias/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:*#
shared_nameAdam/z_mean/bias/v
u
&Adam/z_mean/bias/v/Read/ReadVariableOpReadVariableOpAdam/z_mean/bias/v*
_output_shapes
:*
dtype0

Adam/z_log_sigma/kernel/vVarHandleOp*
_output_shapes
: *
dtype0*
shape
: **
shared_nameAdam/z_log_sigma/kernel/v

-Adam/z_log_sigma/kernel/v/Read/ReadVariableOpReadVariableOpAdam/z_log_sigma/kernel/v*
_output_shapes

: *
dtype0

Adam/z_log_sigma/bias/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:*(
shared_nameAdam/z_log_sigma/bias/v

+Adam/z_log_sigma/bias/v/Read/ReadVariableOpReadVariableOpAdam/z_log_sigma/bias/v*
_output_shapes
:*
dtype0
 
"Adam/classification_layer/kernel/vVarHandleOp*
_output_shapes
: *
dtype0*
shape
: *3
shared_name$"Adam/classification_layer/kernel/v

6Adam/classification_layer/kernel/v/Read/ReadVariableOpReadVariableOp"Adam/classification_layer/kernel/v*
_output_shapes

: *
dtype0

 Adam/classification_layer/bias/vVarHandleOp*
_output_shapes
: *
dtype0*
shape:*1
shared_name" Adam/classification_layer/bias/v

4Adam/classification_layer/bias/v/Read/ReadVariableOpReadVariableOp Adam/classification_layer/bias/v*
_output_shapes
:*
dtype0

NoOpNoOp
¦[
ConstConst"/device:CPU:0*
_output_shapes
: *
dtype0*áZ
value×ZBÔZ BÍZ

layer-0
layer_with_weights-0
layer-1
layer_with_weights-1
layer-2
layer_with_weights-2
layer-3
layer_with_weights-3
layer-4
layer_with_weights-4
layer-5
layer_with_weights-5
layer-6
layer-7
	layer_with_weights-6
	layer-8

	optimizer
loss
	variables
trainable_variables
regularization_losses
	keras_api
__call__
*&call_and_return_all_conditional_losses
_default_save_signature

signatures*
* 
¦

kernel
bias
	variables
trainable_variables
regularization_losses
	keras_api
__call__
*&call_and_return_all_conditional_losses*
¦

kernel
bias
	variables
trainable_variables
 regularization_losses
!	keras_api
"__call__
*#&call_and_return_all_conditional_losses*
¦

$kernel
%bias
&	variables
'trainable_variables
(regularization_losses
)	keras_api
*__call__
*+&call_and_return_all_conditional_losses*
¦

,kernel
-bias
.	variables
/trainable_variables
0regularization_losses
1	keras_api
2__call__
*3&call_and_return_all_conditional_losses*
¦

4kernel
5bias
6	variables
7trainable_variables
8regularization_losses
9	keras_api
:__call__
*;&call_and_return_all_conditional_losses*
¦

<kernel
=bias
>	variables
?trainable_variables
@regularization_losses
A	keras_api
B__call__
*C&call_and_return_all_conditional_losses*

D	variables
Etrainable_variables
Fregularization_losses
G	keras_api
H__call__
*I&call_and_return_all_conditional_losses* 
¦

Jkernel
Kbias
L	variables
Mtrainable_variables
Nregularization_losses
O	keras_api
P__call__
*Q&call_and_return_all_conditional_losses*
Ü

Rbeta_1

Sbeta_2
	Tdecay
Ulearning_rate
Vitermmmm$m%m,m-m4m5m<m=mJmKmvvvv$v%v,v-v4v 5v¡<v¢=v£Jv¤Kv¥*
* 
j
0
1
2
3
$4
%5
,6
-7
48
59
<10
=11
J12
K13*
j
0
1
2
3
$4
%5
,6
-7
48
59
<10
=11
J12
K13*
* 
°
Wnon_trainable_variables

Xlayers
Ymetrics
Zlayer_regularization_losses
[layer_metrics
	variables
trainable_variables
regularization_losses
__call__
_default_save_signature
*&call_and_return_all_conditional_losses
&"call_and_return_conditional_losses*
* 
* 
* 

\serving_default* 
ga
VARIABLE_VALUEEncoding_layer_1/kernel6layer_with_weights-0/kernel/.ATTRIBUTES/VARIABLE_VALUE*
c]
VARIABLE_VALUEEncoding_layer_1/bias4layer_with_weights-0/bias/.ATTRIBUTES/VARIABLE_VALUE*

0
1*

0
1*
* 

]non_trainable_variables

^layers
_metrics
`layer_regularization_losses
alayer_metrics
	variables
trainable_variables
regularization_losses
__call__
*&call_and_return_all_conditional_losses
&"call_and_return_conditional_losses*
* 
* 
ga
VARIABLE_VALUEEncoding_layer_2/kernel6layer_with_weights-1/kernel/.ATTRIBUTES/VARIABLE_VALUE*
c]
VARIABLE_VALUEEncoding_layer_2/bias4layer_with_weights-1/bias/.ATTRIBUTES/VARIABLE_VALUE*

0
1*

0
1*
* 

bnon_trainable_variables

clayers
dmetrics
elayer_regularization_losses
flayer_metrics
	variables
trainable_variables
 regularization_losses
"__call__
*#&call_and_return_all_conditional_losses
&#"call_and_return_conditional_losses*
* 
* 
ga
VARIABLE_VALUEEncoding_layer_3/kernel6layer_with_weights-2/kernel/.ATTRIBUTES/VARIABLE_VALUE*
c]
VARIABLE_VALUEEncoding_layer_3/bias4layer_with_weights-2/bias/.ATTRIBUTES/VARIABLE_VALUE*

$0
%1*

$0
%1*
* 

gnon_trainable_variables

hlayers
imetrics
jlayer_regularization_losses
klayer_metrics
&	variables
'trainable_variables
(regularization_losses
*__call__
*+&call_and_return_all_conditional_losses
&+"call_and_return_conditional_losses*
* 
* 
ga
VARIABLE_VALUEEncoding_layer_4/kernel6layer_with_weights-3/kernel/.ATTRIBUTES/VARIABLE_VALUE*
c]
VARIABLE_VALUEEncoding_layer_4/bias4layer_with_weights-3/bias/.ATTRIBUTES/VARIABLE_VALUE*

,0
-1*

,0
-1*
* 

lnon_trainable_variables

mlayers
nmetrics
olayer_regularization_losses
player_metrics
.	variables
/trainable_variables
0regularization_losses
2__call__
*3&call_and_return_all_conditional_losses
&3"call_and_return_conditional_losses*
* 
* 
]W
VARIABLE_VALUEz_mean/kernel6layer_with_weights-4/kernel/.ATTRIBUTES/VARIABLE_VALUE*
YS
VARIABLE_VALUEz_mean/bias4layer_with_weights-4/bias/.ATTRIBUTES/VARIABLE_VALUE*

40
51*

40
51*
* 

qnon_trainable_variables

rlayers
smetrics
tlayer_regularization_losses
ulayer_metrics
6	variables
7trainable_variables
8regularization_losses
:__call__
*;&call_and_return_all_conditional_losses
&;"call_and_return_conditional_losses*
* 
* 
b\
VARIABLE_VALUEz_log_sigma/kernel6layer_with_weights-5/kernel/.ATTRIBUTES/VARIABLE_VALUE*
^X
VARIABLE_VALUEz_log_sigma/bias4layer_with_weights-5/bias/.ATTRIBUTES/VARIABLE_VALUE*

<0
=1*

<0
=1*
* 

vnon_trainable_variables

wlayers
xmetrics
ylayer_regularization_losses
zlayer_metrics
>	variables
?trainable_variables
@regularization_losses
B__call__
*C&call_and_return_all_conditional_losses
&C"call_and_return_conditional_losses*
* 
* 
* 
* 
* 

{non_trainable_variables

|layers
}metrics
~layer_regularization_losses
layer_metrics
D	variables
Etrainable_variables
Fregularization_losses
H__call__
*I&call_and_return_all_conditional_losses
&I"call_and_return_conditional_losses* 
* 
* 
ke
VARIABLE_VALUEclassification_layer/kernel6layer_with_weights-6/kernel/.ATTRIBUTES/VARIABLE_VALUE*
ga
VARIABLE_VALUEclassification_layer/bias4layer_with_weights-6/bias/.ATTRIBUTES/VARIABLE_VALUE*

J0
K1*

J0
K1*
* 

non_trainable_variables
layers
metrics
 layer_regularization_losses
layer_metrics
L	variables
Mtrainable_variables
Nregularization_losses
P__call__
*Q&call_and_return_all_conditional_losses
&Q"call_and_return_conditional_losses*
* 
* 
KE
VARIABLE_VALUEbeta_1+optimizer/beta_1/.ATTRIBUTES/VARIABLE_VALUE*
KE
VARIABLE_VALUEbeta_2+optimizer/beta_2/.ATTRIBUTES/VARIABLE_VALUE*
IC
VARIABLE_VALUEdecay*optimizer/decay/.ATTRIBUTES/VARIABLE_VALUE*
YS
VARIABLE_VALUElearning_rate2optimizer/learning_rate/.ATTRIBUTES/VARIABLE_VALUE*
LF
VARIABLE_VALUE	Adam/iter)optimizer/iter/.ATTRIBUTES/VARIABLE_VALUE*
* 
C
0
1
2
3
4
5
6
7
	8*

0*
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
* 
<

total

count
	variables
	keras_api*
SM
VARIABLE_VALUEtotal4keras_api/metrics/0/total/.ATTRIBUTES/VARIABLE_VALUE*
SM
VARIABLE_VALUEcount4keras_api/metrics/0/count/.ATTRIBUTES/VARIABLE_VALUE*

0
1*

	variables*

VARIABLE_VALUEAdam/Encoding_layer_1/kernel/mRlayer_with_weights-0/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE*

VARIABLE_VALUEAdam/Encoding_layer_1/bias/mPlayer_with_weights-0/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE*

VARIABLE_VALUEAdam/Encoding_layer_2/kernel/mRlayer_with_weights-1/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE*

VARIABLE_VALUEAdam/Encoding_layer_2/bias/mPlayer_with_weights-1/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE*

VARIABLE_VALUEAdam/Encoding_layer_3/kernel/mRlayer_with_weights-2/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE*

VARIABLE_VALUEAdam/Encoding_layer_3/bias/mPlayer_with_weights-2/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE*

VARIABLE_VALUEAdam/Encoding_layer_4/kernel/mRlayer_with_weights-3/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE*

VARIABLE_VALUEAdam/Encoding_layer_4/bias/mPlayer_with_weights-3/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE*
z
VARIABLE_VALUEAdam/z_mean/kernel/mRlayer_with_weights-4/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE*
|v
VARIABLE_VALUEAdam/z_mean/bias/mPlayer_with_weights-4/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE*

VARIABLE_VALUEAdam/z_log_sigma/kernel/mRlayer_with_weights-5/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE*
{
VARIABLE_VALUEAdam/z_log_sigma/bias/mPlayer_with_weights-5/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE*

VARIABLE_VALUE"Adam/classification_layer/kernel/mRlayer_with_weights-6/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE*

VARIABLE_VALUE Adam/classification_layer/bias/mPlayer_with_weights-6/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUE*

VARIABLE_VALUEAdam/Encoding_layer_1/kernel/vRlayer_with_weights-0/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE*

VARIABLE_VALUEAdam/Encoding_layer_1/bias/vPlayer_with_weights-0/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE*

VARIABLE_VALUEAdam/Encoding_layer_2/kernel/vRlayer_with_weights-1/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE*

VARIABLE_VALUEAdam/Encoding_layer_2/bias/vPlayer_with_weights-1/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE*

VARIABLE_VALUEAdam/Encoding_layer_3/kernel/vRlayer_with_weights-2/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE*

VARIABLE_VALUEAdam/Encoding_layer_3/bias/vPlayer_with_weights-2/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE*

VARIABLE_VALUEAdam/Encoding_layer_4/kernel/vRlayer_with_weights-3/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE*

VARIABLE_VALUEAdam/Encoding_layer_4/bias/vPlayer_with_weights-3/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE*
z
VARIABLE_VALUEAdam/z_mean/kernel/vRlayer_with_weights-4/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE*
|v
VARIABLE_VALUEAdam/z_mean/bias/vPlayer_with_weights-4/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE*

VARIABLE_VALUEAdam/z_log_sigma/kernel/vRlayer_with_weights-5/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE*
{
VARIABLE_VALUEAdam/z_log_sigma/bias/vPlayer_with_weights-5/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE*

VARIABLE_VALUE"Adam/classification_layer/kernel/vRlayer_with_weights-6/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE*

VARIABLE_VALUE Adam/classification_layer/bias/vPlayer_with_weights-6/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUE*
z
serving_default_InputPlaceholder*(
_output_shapes
:ÿÿÿÿÿÿÿÿÿý*
dtype0*
shape:ÿÿÿÿÿÿÿÿÿý
Å
StatefulPartitionedCallStatefulPartitionedCallserving_default_InputEncoding_layer_1/kernelEncoding_layer_1/biasEncoding_layer_2/kernelEncoding_layer_2/biasEncoding_layer_3/kernelEncoding_layer_3/biasEncoding_layer_4/kernelEncoding_layer_4/biasz_mean/kernelz_mean/biasz_log_sigma/kernelz_log_sigma/biasclassification_layer/kernelclassification_layer/bias*
Tin
2*
Tout
2*
_collective_manager_ids
 *`
_output_shapesN
L:ÿÿÿÿÿÿÿÿÿ:ÿÿÿÿÿÿÿÿÿ:ÿÿÿÿÿÿÿÿÿ:ÿÿÿÿÿÿÿÿÿ*0
_read_only_resource_inputs
	
*0
config_proto 

CPU

GPU2*0J 8 *-
f(R&
$__inference_signature_wrapper_459553
O
saver_filenamePlaceholder*
_output_shapes
: *
dtype0*
shape: 
Ô
StatefulPartitionedCall_1StatefulPartitionedCallsaver_filename+Encoding_layer_1/kernel/Read/ReadVariableOp)Encoding_layer_1/bias/Read/ReadVariableOp+Encoding_layer_2/kernel/Read/ReadVariableOp)Encoding_layer_2/bias/Read/ReadVariableOp+Encoding_layer_3/kernel/Read/ReadVariableOp)Encoding_layer_3/bias/Read/ReadVariableOp+Encoding_layer_4/kernel/Read/ReadVariableOp)Encoding_layer_4/bias/Read/ReadVariableOp!z_mean/kernel/Read/ReadVariableOpz_mean/bias/Read/ReadVariableOp&z_log_sigma/kernel/Read/ReadVariableOp$z_log_sigma/bias/Read/ReadVariableOp/classification_layer/kernel/Read/ReadVariableOp-classification_layer/bias/Read/ReadVariableOpbeta_1/Read/ReadVariableOpbeta_2/Read/ReadVariableOpdecay/Read/ReadVariableOp!learning_rate/Read/ReadVariableOpAdam/iter/Read/ReadVariableOptotal/Read/ReadVariableOpcount/Read/ReadVariableOp2Adam/Encoding_layer_1/kernel/m/Read/ReadVariableOp0Adam/Encoding_layer_1/bias/m/Read/ReadVariableOp2Adam/Encoding_layer_2/kernel/m/Read/ReadVariableOp0Adam/Encoding_layer_2/bias/m/Read/ReadVariableOp2Adam/Encoding_layer_3/kernel/m/Read/ReadVariableOp0Adam/Encoding_layer_3/bias/m/Read/ReadVariableOp2Adam/Encoding_layer_4/kernel/m/Read/ReadVariableOp0Adam/Encoding_layer_4/bias/m/Read/ReadVariableOp(Adam/z_mean/kernel/m/Read/ReadVariableOp&Adam/z_mean/bias/m/Read/ReadVariableOp-Adam/z_log_sigma/kernel/m/Read/ReadVariableOp+Adam/z_log_sigma/bias/m/Read/ReadVariableOp6Adam/classification_layer/kernel/m/Read/ReadVariableOp4Adam/classification_layer/bias/m/Read/ReadVariableOp2Adam/Encoding_layer_1/kernel/v/Read/ReadVariableOp0Adam/Encoding_layer_1/bias/v/Read/ReadVariableOp2Adam/Encoding_layer_2/kernel/v/Read/ReadVariableOp0Adam/Encoding_layer_2/bias/v/Read/ReadVariableOp2Adam/Encoding_layer_3/kernel/v/Read/ReadVariableOp0Adam/Encoding_layer_3/bias/v/Read/ReadVariableOp2Adam/Encoding_layer_4/kernel/v/Read/ReadVariableOp0Adam/Encoding_layer_4/bias/v/Read/ReadVariableOp(Adam/z_mean/kernel/v/Read/ReadVariableOp&Adam/z_mean/bias/v/Read/ReadVariableOp-Adam/z_log_sigma/kernel/v/Read/ReadVariableOp+Adam/z_log_sigma/bias/v/Read/ReadVariableOp6Adam/classification_layer/kernel/v/Read/ReadVariableOp4Adam/classification_layer/bias/v/Read/ReadVariableOpConst*>
Tin7
523	*
Tout
2*
_collective_manager_ids
 *
_output_shapes
: * 
_read_only_resource_inputs
 *0
config_proto 

CPU

GPU2*0J 8 *(
f#R!
__inference__traced_save_459922
û
StatefulPartitionedCall_2StatefulPartitionedCallsaver_filenameEncoding_layer_1/kernelEncoding_layer_1/biasEncoding_layer_2/kernelEncoding_layer_2/biasEncoding_layer_3/kernelEncoding_layer_3/biasEncoding_layer_4/kernelEncoding_layer_4/biasz_mean/kernelz_mean/biasz_log_sigma/kernelz_log_sigma/biasclassification_layer/kernelclassification_layer/biasbeta_1beta_2decaylearning_rate	Adam/itertotalcountAdam/Encoding_layer_1/kernel/mAdam/Encoding_layer_1/bias/mAdam/Encoding_layer_2/kernel/mAdam/Encoding_layer_2/bias/mAdam/Encoding_layer_3/kernel/mAdam/Encoding_layer_3/bias/mAdam/Encoding_layer_4/kernel/mAdam/Encoding_layer_4/bias/mAdam/z_mean/kernel/mAdam/z_mean/bias/mAdam/z_log_sigma/kernel/mAdam/z_log_sigma/bias/m"Adam/classification_layer/kernel/m Adam/classification_layer/bias/mAdam/Encoding_layer_1/kernel/vAdam/Encoding_layer_1/bias/vAdam/Encoding_layer_2/kernel/vAdam/Encoding_layer_2/bias/vAdam/Encoding_layer_3/kernel/vAdam/Encoding_layer_3/bias/vAdam/Encoding_layer_4/kernel/vAdam/Encoding_layer_4/bias/vAdam/z_mean/kernel/vAdam/z_mean/bias/vAdam/z_log_sigma/kernel/vAdam/z_log_sigma/bias/v"Adam/classification_layer/kernel/v Adam/classification_layer/bias/v*=
Tin6
422*
Tout
2*
_collective_manager_ids
 *
_output_shapes
: * 
_read_only_resource_inputs
 *0
config_proto 

CPU

GPU2*0J 8 *+
f&R$
"__inference__traced_restore_460079	
Îf

__inference__traced_save_459922
file_prefix6
2savev2_encoding_layer_1_kernel_read_readvariableop4
0savev2_encoding_layer_1_bias_read_readvariableop6
2savev2_encoding_layer_2_kernel_read_readvariableop4
0savev2_encoding_layer_2_bias_read_readvariableop6
2savev2_encoding_layer_3_kernel_read_readvariableop4
0savev2_encoding_layer_3_bias_read_readvariableop6
2savev2_encoding_layer_4_kernel_read_readvariableop4
0savev2_encoding_layer_4_bias_read_readvariableop,
(savev2_z_mean_kernel_read_readvariableop*
&savev2_z_mean_bias_read_readvariableop1
-savev2_z_log_sigma_kernel_read_readvariableop/
+savev2_z_log_sigma_bias_read_readvariableop:
6savev2_classification_layer_kernel_read_readvariableop8
4savev2_classification_layer_bias_read_readvariableop%
!savev2_beta_1_read_readvariableop%
!savev2_beta_2_read_readvariableop$
 savev2_decay_read_readvariableop,
(savev2_learning_rate_read_readvariableop(
$savev2_adam_iter_read_readvariableop	$
 savev2_total_read_readvariableop$
 savev2_count_read_readvariableop=
9savev2_adam_encoding_layer_1_kernel_m_read_readvariableop;
7savev2_adam_encoding_layer_1_bias_m_read_readvariableop=
9savev2_adam_encoding_layer_2_kernel_m_read_readvariableop;
7savev2_adam_encoding_layer_2_bias_m_read_readvariableop=
9savev2_adam_encoding_layer_3_kernel_m_read_readvariableop;
7savev2_adam_encoding_layer_3_bias_m_read_readvariableop=
9savev2_adam_encoding_layer_4_kernel_m_read_readvariableop;
7savev2_adam_encoding_layer_4_bias_m_read_readvariableop3
/savev2_adam_z_mean_kernel_m_read_readvariableop1
-savev2_adam_z_mean_bias_m_read_readvariableop8
4savev2_adam_z_log_sigma_kernel_m_read_readvariableop6
2savev2_adam_z_log_sigma_bias_m_read_readvariableopA
=savev2_adam_classification_layer_kernel_m_read_readvariableop?
;savev2_adam_classification_layer_bias_m_read_readvariableop=
9savev2_adam_encoding_layer_1_kernel_v_read_readvariableop;
7savev2_adam_encoding_layer_1_bias_v_read_readvariableop=
9savev2_adam_encoding_layer_2_kernel_v_read_readvariableop;
7savev2_adam_encoding_layer_2_bias_v_read_readvariableop=
9savev2_adam_encoding_layer_3_kernel_v_read_readvariableop;
7savev2_adam_encoding_layer_3_bias_v_read_readvariableop=
9savev2_adam_encoding_layer_4_kernel_v_read_readvariableop;
7savev2_adam_encoding_layer_4_bias_v_read_readvariableop3
/savev2_adam_z_mean_kernel_v_read_readvariableop1
-savev2_adam_z_mean_bias_v_read_readvariableop8
4savev2_adam_z_log_sigma_kernel_v_read_readvariableop6
2savev2_adam_z_log_sigma_bias_v_read_readvariableopA
=savev2_adam_classification_layer_kernel_v_read_readvariableop?
;savev2_adam_classification_layer_bias_v_read_readvariableop
savev2_const

identity_1¢MergeV2Checkpointsw
StaticRegexFullMatchStaticRegexFullMatchfile_prefix"/device:CPU:**
_output_shapes
: *
pattern
^s3://.*Z
ConstConst"/device:CPU:**
_output_shapes
: *
dtype0*
valueB B.parta
Const_1Const"/device:CPU:**
_output_shapes
: *
dtype0*
valueB B
_temp/part
SelectSelectStaticRegexFullMatch:output:0Const:output:0Const_1:output:0"/device:CPU:**
T0*
_output_shapes
: f

StringJoin
StringJoinfile_prefixSelect:output:0"/device:CPU:**
N*
_output_shapes
: L

num_shardsConst*
_output_shapes
: *
dtype0*
value	B :f
ShardedFilename/shardConst"/device:CPU:0*
_output_shapes
: *
dtype0*
value	B : 
ShardedFilenameShardedFilenameStringJoin:output:0ShardedFilename/shard:output:0num_shards:output:0"/device:CPU:0*
_output_shapes
: ñ
SaveV2/tensor_namesConst"/device:CPU:0*
_output_shapes
:2*
dtype0*
valueB2B6layer_with_weights-0/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-0/bias/.ATTRIBUTES/VARIABLE_VALUEB6layer_with_weights-1/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-1/bias/.ATTRIBUTES/VARIABLE_VALUEB6layer_with_weights-2/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-2/bias/.ATTRIBUTES/VARIABLE_VALUEB6layer_with_weights-3/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-3/bias/.ATTRIBUTES/VARIABLE_VALUEB6layer_with_weights-4/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-4/bias/.ATTRIBUTES/VARIABLE_VALUEB6layer_with_weights-5/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-5/bias/.ATTRIBUTES/VARIABLE_VALUEB6layer_with_weights-6/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-6/bias/.ATTRIBUTES/VARIABLE_VALUEB+optimizer/beta_1/.ATTRIBUTES/VARIABLE_VALUEB+optimizer/beta_2/.ATTRIBUTES/VARIABLE_VALUEB*optimizer/decay/.ATTRIBUTES/VARIABLE_VALUEB2optimizer/learning_rate/.ATTRIBUTES/VARIABLE_VALUEB)optimizer/iter/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/count/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-0/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-0/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-1/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-1/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-2/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-2/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-3/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-3/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-4/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-4/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-5/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-5/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-6/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-6/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-0/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-0/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-1/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-1/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-2/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-2/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-3/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-3/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-4/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-4/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-5/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-5/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-6/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-6/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEB_CHECKPOINTABLE_OBJECT_GRAPHÑ
SaveV2/shape_and_slicesConst"/device:CPU:0*
_output_shapes
:2*
dtype0*w
valuenBl2B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B ½
SaveV2SaveV2ShardedFilename:filename:0SaveV2/tensor_names:output:0 SaveV2/shape_and_slices:output:02savev2_encoding_layer_1_kernel_read_readvariableop0savev2_encoding_layer_1_bias_read_readvariableop2savev2_encoding_layer_2_kernel_read_readvariableop0savev2_encoding_layer_2_bias_read_readvariableop2savev2_encoding_layer_3_kernel_read_readvariableop0savev2_encoding_layer_3_bias_read_readvariableop2savev2_encoding_layer_4_kernel_read_readvariableop0savev2_encoding_layer_4_bias_read_readvariableop(savev2_z_mean_kernel_read_readvariableop&savev2_z_mean_bias_read_readvariableop-savev2_z_log_sigma_kernel_read_readvariableop+savev2_z_log_sigma_bias_read_readvariableop6savev2_classification_layer_kernel_read_readvariableop4savev2_classification_layer_bias_read_readvariableop!savev2_beta_1_read_readvariableop!savev2_beta_2_read_readvariableop savev2_decay_read_readvariableop(savev2_learning_rate_read_readvariableop$savev2_adam_iter_read_readvariableop savev2_total_read_readvariableop savev2_count_read_readvariableop9savev2_adam_encoding_layer_1_kernel_m_read_readvariableop7savev2_adam_encoding_layer_1_bias_m_read_readvariableop9savev2_adam_encoding_layer_2_kernel_m_read_readvariableop7savev2_adam_encoding_layer_2_bias_m_read_readvariableop9savev2_adam_encoding_layer_3_kernel_m_read_readvariableop7savev2_adam_encoding_layer_3_bias_m_read_readvariableop9savev2_adam_encoding_layer_4_kernel_m_read_readvariableop7savev2_adam_encoding_layer_4_bias_m_read_readvariableop/savev2_adam_z_mean_kernel_m_read_readvariableop-savev2_adam_z_mean_bias_m_read_readvariableop4savev2_adam_z_log_sigma_kernel_m_read_readvariableop2savev2_adam_z_log_sigma_bias_m_read_readvariableop=savev2_adam_classification_layer_kernel_m_read_readvariableop;savev2_adam_classification_layer_bias_m_read_readvariableop9savev2_adam_encoding_layer_1_kernel_v_read_readvariableop7savev2_adam_encoding_layer_1_bias_v_read_readvariableop9savev2_adam_encoding_layer_2_kernel_v_read_readvariableop7savev2_adam_encoding_layer_2_bias_v_read_readvariableop9savev2_adam_encoding_layer_3_kernel_v_read_readvariableop7savev2_adam_encoding_layer_3_bias_v_read_readvariableop9savev2_adam_encoding_layer_4_kernel_v_read_readvariableop7savev2_adam_encoding_layer_4_bias_v_read_readvariableop/savev2_adam_z_mean_kernel_v_read_readvariableop-savev2_adam_z_mean_bias_v_read_readvariableop4savev2_adam_z_log_sigma_kernel_v_read_readvariableop2savev2_adam_z_log_sigma_bias_v_read_readvariableop=savev2_adam_classification_layer_kernel_v_read_readvariableop;savev2_adam_classification_layer_bias_v_read_readvariableopsavev2_const"/device:CPU:0*
_output_shapes
 *@
dtypes6
422	
&MergeV2Checkpoints/checkpoint_prefixesPackShardedFilename:filename:0^SaveV2"/device:CPU:0*
N*
T0*
_output_shapes
:
MergeV2CheckpointsMergeV2Checkpoints/MergeV2Checkpoints/checkpoint_prefixes:output:0file_prefix"/device:CPU:0*
_output_shapes
 f
IdentityIdentityfile_prefix^MergeV2Checkpoints"/device:CPU:0*
T0*
_output_shapes
: Q

Identity_1IdentityIdentity:output:0^NoOp*
T0*
_output_shapes
: [
NoOpNoOp^MergeV2Checkpoints*"
_acd_function_control_output(*
_output_shapes
 "!

identity_1Identity_1:output:0*
_input_shapesú
÷: :
ý::
::	@:@:@ : : :: :: :: : : : : : : :
ý::
::	@:@:@ : : :: :: ::
ý::
::	@:@:@ : : :: :: :: 2(
MergeV2CheckpointsMergeV2Checkpoints:C ?

_output_shapes
: 
%
_user_specified_namefile_prefix:&"
 
_output_shapes
:
ý:!

_output_shapes	
::&"
 
_output_shapes
:
:!

_output_shapes	
::%!

_output_shapes
:	@: 

_output_shapes
:@:$ 

_output_shapes

:@ : 

_output_shapes
: :$	 

_output_shapes

: : 


_output_shapes
::$ 

_output_shapes

: : 

_output_shapes
::$ 

_output_shapes

: : 

_output_shapes
::

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :

_output_shapes
: :&"
 
_output_shapes
:
ý:!

_output_shapes	
::&"
 
_output_shapes
:
:!

_output_shapes	
::%!

_output_shapes
:	@: 

_output_shapes
:@:$ 

_output_shapes

:@ : 

_output_shapes
: :$ 

_output_shapes

: : 

_output_shapes
::$  

_output_shapes

: : !

_output_shapes
::$" 

_output_shapes

: : #

_output_shapes
::&$"
 
_output_shapes
:
ý:!%

_output_shapes	
::&&"
 
_output_shapes
:
:!'

_output_shapes	
::%(!

_output_shapes
:	@: )

_output_shapes
:@:$* 

_output_shapes

:@ : +

_output_shapes
: :$, 

_output_shapes

: : -

_output_shapes
::$. 

_output_shapes

: : /

_output_shapes
::$0 

_output_shapes

: : 1

_output_shapes
::2

_output_shapes
: 
÷1

G__inference_VAE-encoder_layer_call_and_return_conditional_losses_459241	
input+
encoding_layer_1_459201:
ý&
encoding_layer_1_459203:	+
encoding_layer_2_459206:
&
encoding_layer_2_459208:	*
encoding_layer_3_459211:	@%
encoding_layer_3_459213:@)
encoding_layer_4_459216:@ %
encoding_layer_4_459218: 
z_mean_459221: 
z_mean_459223:$
z_log_sigma_459226:  
z_log_sigma_459228:-
classification_layer_459231: )
classification_layer_459233:
identity

identity_1

identity_2

identity_3¢(Encoding_layer_1/StatefulPartitionedCall¢(Encoding_layer_2/StatefulPartitionedCall¢(Encoding_layer_3/StatefulPartitionedCall¢(Encoding_layer_4/StatefulPartitionedCall¢ Sampling/StatefulPartitionedCall¢,classification_layer/StatefulPartitionedCall¢#z_log_sigma/StatefulPartitionedCall¢z_mean/StatefulPartitionedCall
(Encoding_layer_1/StatefulPartitionedCallStatefulPartitionedCallinputencoding_layer_1_459201encoding_layer_1_459203*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:ÿÿÿÿÿÿÿÿÿ*$
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8 *U
fPRN
L__inference_Encoding_layer_1_layer_call_and_return_conditional_losses_458760¿
(Encoding_layer_2/StatefulPartitionedCallStatefulPartitionedCall1Encoding_layer_1/StatefulPartitionedCall:output:0encoding_layer_2_459206encoding_layer_2_459208*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:ÿÿÿÿÿÿÿÿÿ*$
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8 *U
fPRN
L__inference_Encoding_layer_2_layer_call_and_return_conditional_losses_458777¾
(Encoding_layer_3/StatefulPartitionedCallStatefulPartitionedCall1Encoding_layer_2/StatefulPartitionedCall:output:0encoding_layer_3_459211encoding_layer_3_459213*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ@*$
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8 *U
fPRN
L__inference_Encoding_layer_3_layer_call_and_return_conditional_losses_458794¾
(Encoding_layer_4/StatefulPartitionedCallStatefulPartitionedCall1Encoding_layer_3/StatefulPartitionedCall:output:0encoding_layer_4_459216encoding_layer_4_459218*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ *$
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8 *U
fPRN
L__inference_Encoding_layer_4_layer_call_and_return_conditional_losses_458811
z_mean/StatefulPartitionedCallStatefulPartitionedCall1Encoding_layer_4/StatefulPartitionedCall:output:0z_mean_459221z_mean_459223*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ*$
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8 *K
fFRD
B__inference_z_mean_layer_call_and_return_conditional_losses_458827ª
#z_log_sigma/StatefulPartitionedCallStatefulPartitionedCall1Encoding_layer_4/StatefulPartitionedCall:output:0z_log_sigma_459226z_log_sigma_459228*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ*$
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8 *P
fKRI
G__inference_z_log_sigma_layer_call_and_return_conditional_losses_458843Î
,classification_layer/StatefulPartitionedCallStatefulPartitionedCall1Encoding_layer_4/StatefulPartitionedCall:output:0classification_layer_459231classification_layer_459233*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ*$
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8 *Y
fTRR
P__inference_classification_layer_layer_call_and_return_conditional_losses_458860
 Sampling/StatefulPartitionedCallStatefulPartitionedCall'z_mean/StatefulPartitionedCall:output:0,z_log_sigma/StatefulPartitionedCall:output:0*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ* 
_read_only_resource_inputs
 *0
config_proto 

CPU

GPU2*0J 8 *M
fHRF
D__inference_Sampling_layer_call_and_return_conditional_losses_458889v
IdentityIdentity'z_mean/StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ}

Identity_1Identity,z_log_sigma/StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿz

Identity_2Identity)Sampling/StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ

Identity_3Identity5classification_layer/StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
NoOpNoOp)^Encoding_layer_1/StatefulPartitionedCall)^Encoding_layer_2/StatefulPartitionedCall)^Encoding_layer_3/StatefulPartitionedCall)^Encoding_layer_4/StatefulPartitionedCall!^Sampling/StatefulPartitionedCall-^classification_layer/StatefulPartitionedCall$^z_log_sigma/StatefulPartitionedCall^z_mean/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0*(
_construction_contextkEagerRuntime*C
_input_shapes2
0:ÿÿÿÿÿÿÿÿÿý: : : : : : : : : : : : : : 2T
(Encoding_layer_1/StatefulPartitionedCall(Encoding_layer_1/StatefulPartitionedCall2T
(Encoding_layer_2/StatefulPartitionedCall(Encoding_layer_2/StatefulPartitionedCall2T
(Encoding_layer_3/StatefulPartitionedCall(Encoding_layer_3/StatefulPartitionedCall2T
(Encoding_layer_4/StatefulPartitionedCall(Encoding_layer_4/StatefulPartitionedCall2D
 Sampling/StatefulPartitionedCall Sampling/StatefulPartitionedCall2\
,classification_layer/StatefulPartitionedCall,classification_layer/StatefulPartitionedCall2J
#z_log_sigma/StatefulPartitionedCall#z_log_sigma/StatefulPartitionedCall2@
z_mean/StatefulPartitionedCallz_mean/StatefulPartitionedCall:O K
(
_output_shapes
:ÿÿÿÿÿÿÿÿÿý

_user_specified_nameInput
Ê	
ø
G__inference_z_log_sigma_layer_call_and_return_conditional_losses_459671

inputs0
matmul_readvariableop_resource: -
biasadd_readvariableop_resource:
identity¢BiasAdd/ReadVariableOp¢MatMul/ReadVariableOpt
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

: *
dtype0i
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿr
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:*
dtype0v
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ_
IdentityIdentityBiasAdd:output:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿw
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:ÿÿÿÿÿÿÿÿÿ : : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:O K
'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ 
 
_user_specified_nameinputs
ú1

G__inference_VAE-encoder_layer_call_and_return_conditional_losses_459122

inputs+
encoding_layer_1_459082:
ý&
encoding_layer_1_459084:	+
encoding_layer_2_459087:
&
encoding_layer_2_459089:	*
encoding_layer_3_459092:	@%
encoding_layer_3_459094:@)
encoding_layer_4_459097:@ %
encoding_layer_4_459099: 
z_mean_459102: 
z_mean_459104:$
z_log_sigma_459107:  
z_log_sigma_459109:-
classification_layer_459112: )
classification_layer_459114:
identity

identity_1

identity_2

identity_3¢(Encoding_layer_1/StatefulPartitionedCall¢(Encoding_layer_2/StatefulPartitionedCall¢(Encoding_layer_3/StatefulPartitionedCall¢(Encoding_layer_4/StatefulPartitionedCall¢ Sampling/StatefulPartitionedCall¢,classification_layer/StatefulPartitionedCall¢#z_log_sigma/StatefulPartitionedCall¢z_mean/StatefulPartitionedCall
(Encoding_layer_1/StatefulPartitionedCallStatefulPartitionedCallinputsencoding_layer_1_459082encoding_layer_1_459084*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:ÿÿÿÿÿÿÿÿÿ*$
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8 *U
fPRN
L__inference_Encoding_layer_1_layer_call_and_return_conditional_losses_458760¿
(Encoding_layer_2/StatefulPartitionedCallStatefulPartitionedCall1Encoding_layer_1/StatefulPartitionedCall:output:0encoding_layer_2_459087encoding_layer_2_459089*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:ÿÿÿÿÿÿÿÿÿ*$
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8 *U
fPRN
L__inference_Encoding_layer_2_layer_call_and_return_conditional_losses_458777¾
(Encoding_layer_3/StatefulPartitionedCallStatefulPartitionedCall1Encoding_layer_2/StatefulPartitionedCall:output:0encoding_layer_3_459092encoding_layer_3_459094*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ@*$
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8 *U
fPRN
L__inference_Encoding_layer_3_layer_call_and_return_conditional_losses_458794¾
(Encoding_layer_4/StatefulPartitionedCallStatefulPartitionedCall1Encoding_layer_3/StatefulPartitionedCall:output:0encoding_layer_4_459097encoding_layer_4_459099*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ *$
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8 *U
fPRN
L__inference_Encoding_layer_4_layer_call_and_return_conditional_losses_458811
z_mean/StatefulPartitionedCallStatefulPartitionedCall1Encoding_layer_4/StatefulPartitionedCall:output:0z_mean_459102z_mean_459104*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ*$
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8 *K
fFRD
B__inference_z_mean_layer_call_and_return_conditional_losses_458827ª
#z_log_sigma/StatefulPartitionedCallStatefulPartitionedCall1Encoding_layer_4/StatefulPartitionedCall:output:0z_log_sigma_459107z_log_sigma_459109*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ*$
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8 *P
fKRI
G__inference_z_log_sigma_layer_call_and_return_conditional_losses_458843Î
,classification_layer/StatefulPartitionedCallStatefulPartitionedCall1Encoding_layer_4/StatefulPartitionedCall:output:0classification_layer_459112classification_layer_459114*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ*$
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8 *Y
fTRR
P__inference_classification_layer_layer_call_and_return_conditional_losses_458860
 Sampling/StatefulPartitionedCallStatefulPartitionedCall'z_mean/StatefulPartitionedCall:output:0,z_log_sigma/StatefulPartitionedCall:output:0*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ* 
_read_only_resource_inputs
 *0
config_proto 

CPU

GPU2*0J 8 *M
fHRF
D__inference_Sampling_layer_call_and_return_conditional_losses_458965v
IdentityIdentity'z_mean/StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ}

Identity_1Identity,z_log_sigma/StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿz

Identity_2Identity)Sampling/StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ

Identity_3Identity5classification_layer/StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
NoOpNoOp)^Encoding_layer_1/StatefulPartitionedCall)^Encoding_layer_2/StatefulPartitionedCall)^Encoding_layer_3/StatefulPartitionedCall)^Encoding_layer_4/StatefulPartitionedCall!^Sampling/StatefulPartitionedCall-^classification_layer/StatefulPartitionedCall$^z_log_sigma/StatefulPartitionedCall^z_mean/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0*(
_construction_contextkEagerRuntime*C
_input_shapes2
0:ÿÿÿÿÿÿÿÿÿý: : : : : : : : : : : : : : 2T
(Encoding_layer_1/StatefulPartitionedCall(Encoding_layer_1/StatefulPartitionedCall2T
(Encoding_layer_2/StatefulPartitionedCall(Encoding_layer_2/StatefulPartitionedCall2T
(Encoding_layer_3/StatefulPartitionedCall(Encoding_layer_3/StatefulPartitionedCall2T
(Encoding_layer_4/StatefulPartitionedCall(Encoding_layer_4/StatefulPartitionedCall2D
 Sampling/StatefulPartitionedCall Sampling/StatefulPartitionedCall2\
,classification_layer/StatefulPartitionedCall,classification_layer/StatefulPartitionedCall2J
#z_log_sigma/StatefulPartitionedCall#z_log_sigma/StatefulPartitionedCall2@
z_mean/StatefulPartitionedCallz_mean/StatefulPartitionedCall:P L
(
_output_shapes
:ÿÿÿÿÿÿÿÿÿý
 
_user_specified_nameinputs
ûY
Ë
G__inference_VAE-encoder_layer_call_and_return_conditional_losses_459440

inputsC
/encoding_layer_1_matmul_readvariableop_resource:
ý?
0encoding_layer_1_biasadd_readvariableop_resource:	C
/encoding_layer_2_matmul_readvariableop_resource:
?
0encoding_layer_2_biasadd_readvariableop_resource:	B
/encoding_layer_3_matmul_readvariableop_resource:	@>
0encoding_layer_3_biasadd_readvariableop_resource:@A
/encoding_layer_4_matmul_readvariableop_resource:@ >
0encoding_layer_4_biasadd_readvariableop_resource: 7
%z_mean_matmul_readvariableop_resource: 4
&z_mean_biasadd_readvariableop_resource:<
*z_log_sigma_matmul_readvariableop_resource: 9
+z_log_sigma_biasadd_readvariableop_resource:E
3classification_layer_matmul_readvariableop_resource: B
4classification_layer_biasadd_readvariableop_resource:
identity

identity_1

identity_2

identity_3¢'Encoding_layer_1/BiasAdd/ReadVariableOp¢&Encoding_layer_1/MatMul/ReadVariableOp¢'Encoding_layer_2/BiasAdd/ReadVariableOp¢&Encoding_layer_2/MatMul/ReadVariableOp¢'Encoding_layer_3/BiasAdd/ReadVariableOp¢&Encoding_layer_3/MatMul/ReadVariableOp¢'Encoding_layer_4/BiasAdd/ReadVariableOp¢&Encoding_layer_4/MatMul/ReadVariableOp¢+classification_layer/BiasAdd/ReadVariableOp¢*classification_layer/MatMul/ReadVariableOp¢"z_log_sigma/BiasAdd/ReadVariableOp¢!z_log_sigma/MatMul/ReadVariableOp¢z_mean/BiasAdd/ReadVariableOp¢z_mean/MatMul/ReadVariableOp
&Encoding_layer_1/MatMul/ReadVariableOpReadVariableOp/encoding_layer_1_matmul_readvariableop_resource* 
_output_shapes
:
ý*
dtype0
Encoding_layer_1/MatMulMatMulinputs.Encoding_layer_1/MatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
'Encoding_layer_1/BiasAdd/ReadVariableOpReadVariableOp0encoding_layer_1_biasadd_readvariableop_resource*
_output_shapes	
:*
dtype0ª
Encoding_layer_1/BiasAddBiasAdd!Encoding_layer_1/MatMul:product:0/Encoding_layer_1/BiasAdd/ReadVariableOp:value:0*
T0*(
_output_shapes
:ÿÿÿÿÿÿÿÿÿs
Encoding_layer_1/TanhTanh!Encoding_layer_1/BiasAdd:output:0*
T0*(
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
&Encoding_layer_2/MatMul/ReadVariableOpReadVariableOp/encoding_layer_2_matmul_readvariableop_resource* 
_output_shapes
:
*
dtype0
Encoding_layer_2/MatMulMatMulEncoding_layer_1/Tanh:y:0.Encoding_layer_2/MatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
'Encoding_layer_2/BiasAdd/ReadVariableOpReadVariableOp0encoding_layer_2_biasadd_readvariableop_resource*
_output_shapes	
:*
dtype0ª
Encoding_layer_2/BiasAddBiasAdd!Encoding_layer_2/MatMul:product:0/Encoding_layer_2/BiasAdd/ReadVariableOp:value:0*
T0*(
_output_shapes
:ÿÿÿÿÿÿÿÿÿs
Encoding_layer_2/TanhTanh!Encoding_layer_2/BiasAdd:output:0*
T0*(
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
&Encoding_layer_3/MatMul/ReadVariableOpReadVariableOp/encoding_layer_3_matmul_readvariableop_resource*
_output_shapes
:	@*
dtype0
Encoding_layer_3/MatMulMatMulEncoding_layer_2/Tanh:y:0.Encoding_layer_3/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ@
'Encoding_layer_3/BiasAdd/ReadVariableOpReadVariableOp0encoding_layer_3_biasadd_readvariableop_resource*
_output_shapes
:@*
dtype0©
Encoding_layer_3/BiasAddBiasAdd!Encoding_layer_3/MatMul:product:0/Encoding_layer_3/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ@r
Encoding_layer_3/TanhTanh!Encoding_layer_3/BiasAdd:output:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ@
&Encoding_layer_4/MatMul/ReadVariableOpReadVariableOp/encoding_layer_4_matmul_readvariableop_resource*
_output_shapes

:@ *
dtype0
Encoding_layer_4/MatMulMatMulEncoding_layer_3/Tanh:y:0.Encoding_layer_4/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ 
'Encoding_layer_4/BiasAdd/ReadVariableOpReadVariableOp0encoding_layer_4_biasadd_readvariableop_resource*
_output_shapes
: *
dtype0©
Encoding_layer_4/BiasAddBiasAdd!Encoding_layer_4/MatMul:product:0/Encoding_layer_4/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ r
Encoding_layer_4/TanhTanh!Encoding_layer_4/BiasAdd:output:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ 
z_mean/MatMul/ReadVariableOpReadVariableOp%z_mean_matmul_readvariableop_resource*
_output_shapes

: *
dtype0
z_mean/MatMulMatMulEncoding_layer_4/Tanh:y:0$z_mean/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
z_mean/BiasAdd/ReadVariableOpReadVariableOp&z_mean_biasadd_readvariableop_resource*
_output_shapes
:*
dtype0
z_mean/BiasAddBiasAddz_mean/MatMul:product:0%z_mean/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
!z_log_sigma/MatMul/ReadVariableOpReadVariableOp*z_log_sigma_matmul_readvariableop_resource*
_output_shapes

: *
dtype0
z_log_sigma/MatMulMatMulEncoding_layer_4/Tanh:y:0)z_log_sigma/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
"z_log_sigma/BiasAdd/ReadVariableOpReadVariableOp+z_log_sigma_biasadd_readvariableop_resource*
_output_shapes
:*
dtype0
z_log_sigma/BiasAddBiasAddz_log_sigma/MatMul:product:0*z_log_sigma/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
*classification_layer/MatMul/ReadVariableOpReadVariableOp3classification_layer_matmul_readvariableop_resource*
_output_shapes

: *
dtype0¦
classification_layer/MatMulMatMulEncoding_layer_4/Tanh:y:02classification_layer/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
+classification_layer/BiasAdd/ReadVariableOpReadVariableOp4classification_layer_biasadd_readvariableop_resource*
_output_shapes
:*
dtype0µ
classification_layer/BiasAddBiasAdd%classification_layer/MatMul:product:03classification_layer/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
classification_layer/SoftmaxSoftmax%classification_layer/BiasAdd:output:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿU
Sampling/ShapeShapez_mean/BiasAdd:output:0*
T0*
_output_shapes
:f
Sampling/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: h
Sampling/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:h
Sampling/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:þ
Sampling/strided_sliceStridedSliceSampling/Shape:output:0%Sampling/strided_slice/stack:output:0'Sampling/strided_slice/stack_1:output:0'Sampling/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_maskh
Sampling/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB:j
 Sampling/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:j
 Sampling/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:
Sampling/strided_slice_1StridedSliceSampling/Shape:output:0'Sampling/strided_slice_1/stack:output:0)Sampling/strided_slice_1/stack_1:output:0)Sampling/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask
Sampling/random_normal/shapePackSampling/strided_slice:output:0!Sampling/strided_slice_1:output:0*
N*
T0*
_output_shapes
:`
Sampling/random_normal/meanConst*
_output_shapes
: *
dtype0*
valueB
 *    b
Sampling/random_normal/stddevConst*
_output_shapes
: *
dtype0*
valueB
 *
×£;©
+Sampling/random_normal/RandomStandardNormalRandomStandardNormal%Sampling/random_normal/shape:output:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ*
dtype0±
Sampling/random_normal/mulMul4Sampling/random_normal/RandomStandardNormal:output:0&Sampling/random_normal/stddev:output:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
Sampling/random_normalAddV2Sampling/random_normal/mul:z:0$Sampling/random_normal/mean:output:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿc
Sampling/ExpExpz_log_sigma/BiasAdd:output:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿs
Sampling/mulMulSampling/Exp:y:0Sampling/random_normal:z:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿr
Sampling/addAddV2z_mean/BiasAdd:output:0Sampling/mul:z:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿf
IdentityIdentityz_mean/BiasAdd:output:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿm

Identity_1Identityz_log_sigma/BiasAdd:output:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿa

Identity_2IdentitySampling/add:z:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿw

Identity_3Identity&classification_layer/Softmax:softmax:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿõ
NoOpNoOp(^Encoding_layer_1/BiasAdd/ReadVariableOp'^Encoding_layer_1/MatMul/ReadVariableOp(^Encoding_layer_2/BiasAdd/ReadVariableOp'^Encoding_layer_2/MatMul/ReadVariableOp(^Encoding_layer_3/BiasAdd/ReadVariableOp'^Encoding_layer_3/MatMul/ReadVariableOp(^Encoding_layer_4/BiasAdd/ReadVariableOp'^Encoding_layer_4/MatMul/ReadVariableOp,^classification_layer/BiasAdd/ReadVariableOp+^classification_layer/MatMul/ReadVariableOp#^z_log_sigma/BiasAdd/ReadVariableOp"^z_log_sigma/MatMul/ReadVariableOp^z_mean/BiasAdd/ReadVariableOp^z_mean/MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0*(
_construction_contextkEagerRuntime*C
_input_shapes2
0:ÿÿÿÿÿÿÿÿÿý: : : : : : : : : : : : : : 2R
'Encoding_layer_1/BiasAdd/ReadVariableOp'Encoding_layer_1/BiasAdd/ReadVariableOp2P
&Encoding_layer_1/MatMul/ReadVariableOp&Encoding_layer_1/MatMul/ReadVariableOp2R
'Encoding_layer_2/BiasAdd/ReadVariableOp'Encoding_layer_2/BiasAdd/ReadVariableOp2P
&Encoding_layer_2/MatMul/ReadVariableOp&Encoding_layer_2/MatMul/ReadVariableOp2R
'Encoding_layer_3/BiasAdd/ReadVariableOp'Encoding_layer_3/BiasAdd/ReadVariableOp2P
&Encoding_layer_3/MatMul/ReadVariableOp&Encoding_layer_3/MatMul/ReadVariableOp2R
'Encoding_layer_4/BiasAdd/ReadVariableOp'Encoding_layer_4/BiasAdd/ReadVariableOp2P
&Encoding_layer_4/MatMul/ReadVariableOp&Encoding_layer_4/MatMul/ReadVariableOp2Z
+classification_layer/BiasAdd/ReadVariableOp+classification_layer/BiasAdd/ReadVariableOp2X
*classification_layer/MatMul/ReadVariableOp*classification_layer/MatMul/ReadVariableOp2H
"z_log_sigma/BiasAdd/ReadVariableOp"z_log_sigma/BiasAdd/ReadVariableOp2F
!z_log_sigma/MatMul/ReadVariableOp!z_log_sigma/MatMul/ReadVariableOp2>
z_mean/BiasAdd/ReadVariableOpz_mean/BiasAdd/ReadVariableOp2<
z_mean/MatMul/ReadVariableOpz_mean/MatMul/ReadVariableOp:P L
(
_output_shapes
:ÿÿÿÿÿÿÿÿÿý
 
_user_specified_nameinputs
ÄÇ
ç
"__inference__traced_restore_460079
file_prefix<
(assignvariableop_encoding_layer_1_kernel:
ý7
(assignvariableop_1_encoding_layer_1_bias:	>
*assignvariableop_2_encoding_layer_2_kernel:
7
(assignvariableop_3_encoding_layer_2_bias:	=
*assignvariableop_4_encoding_layer_3_kernel:	@6
(assignvariableop_5_encoding_layer_3_bias:@<
*assignvariableop_6_encoding_layer_4_kernel:@ 6
(assignvariableop_7_encoding_layer_4_bias: 2
 assignvariableop_8_z_mean_kernel: ,
assignvariableop_9_z_mean_bias:8
&assignvariableop_10_z_log_sigma_kernel: 2
$assignvariableop_11_z_log_sigma_bias:A
/assignvariableop_12_classification_layer_kernel: ;
-assignvariableop_13_classification_layer_bias:$
assignvariableop_14_beta_1: $
assignvariableop_15_beta_2: #
assignvariableop_16_decay: +
!assignvariableop_17_learning_rate: '
assignvariableop_18_adam_iter:	 #
assignvariableop_19_total: #
assignvariableop_20_count: F
2assignvariableop_21_adam_encoding_layer_1_kernel_m:
ý?
0assignvariableop_22_adam_encoding_layer_1_bias_m:	F
2assignvariableop_23_adam_encoding_layer_2_kernel_m:
?
0assignvariableop_24_adam_encoding_layer_2_bias_m:	E
2assignvariableop_25_adam_encoding_layer_3_kernel_m:	@>
0assignvariableop_26_adam_encoding_layer_3_bias_m:@D
2assignvariableop_27_adam_encoding_layer_4_kernel_m:@ >
0assignvariableop_28_adam_encoding_layer_4_bias_m: :
(assignvariableop_29_adam_z_mean_kernel_m: 4
&assignvariableop_30_adam_z_mean_bias_m:?
-assignvariableop_31_adam_z_log_sigma_kernel_m: 9
+assignvariableop_32_adam_z_log_sigma_bias_m:H
6assignvariableop_33_adam_classification_layer_kernel_m: B
4assignvariableop_34_adam_classification_layer_bias_m:F
2assignvariableop_35_adam_encoding_layer_1_kernel_v:
ý?
0assignvariableop_36_adam_encoding_layer_1_bias_v:	F
2assignvariableop_37_adam_encoding_layer_2_kernel_v:
?
0assignvariableop_38_adam_encoding_layer_2_bias_v:	E
2assignvariableop_39_adam_encoding_layer_3_kernel_v:	@>
0assignvariableop_40_adam_encoding_layer_3_bias_v:@D
2assignvariableop_41_adam_encoding_layer_4_kernel_v:@ >
0assignvariableop_42_adam_encoding_layer_4_bias_v: :
(assignvariableop_43_adam_z_mean_kernel_v: 4
&assignvariableop_44_adam_z_mean_bias_v:?
-assignvariableop_45_adam_z_log_sigma_kernel_v: 9
+assignvariableop_46_adam_z_log_sigma_bias_v:H
6assignvariableop_47_adam_classification_layer_kernel_v: B
4assignvariableop_48_adam_classification_layer_bias_v:
identity_50¢AssignVariableOp¢AssignVariableOp_1¢AssignVariableOp_10¢AssignVariableOp_11¢AssignVariableOp_12¢AssignVariableOp_13¢AssignVariableOp_14¢AssignVariableOp_15¢AssignVariableOp_16¢AssignVariableOp_17¢AssignVariableOp_18¢AssignVariableOp_19¢AssignVariableOp_2¢AssignVariableOp_20¢AssignVariableOp_21¢AssignVariableOp_22¢AssignVariableOp_23¢AssignVariableOp_24¢AssignVariableOp_25¢AssignVariableOp_26¢AssignVariableOp_27¢AssignVariableOp_28¢AssignVariableOp_29¢AssignVariableOp_3¢AssignVariableOp_30¢AssignVariableOp_31¢AssignVariableOp_32¢AssignVariableOp_33¢AssignVariableOp_34¢AssignVariableOp_35¢AssignVariableOp_36¢AssignVariableOp_37¢AssignVariableOp_38¢AssignVariableOp_39¢AssignVariableOp_4¢AssignVariableOp_40¢AssignVariableOp_41¢AssignVariableOp_42¢AssignVariableOp_43¢AssignVariableOp_44¢AssignVariableOp_45¢AssignVariableOp_46¢AssignVariableOp_47¢AssignVariableOp_48¢AssignVariableOp_5¢AssignVariableOp_6¢AssignVariableOp_7¢AssignVariableOp_8¢AssignVariableOp_9ô
RestoreV2/tensor_namesConst"/device:CPU:0*
_output_shapes
:2*
dtype0*
valueB2B6layer_with_weights-0/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-0/bias/.ATTRIBUTES/VARIABLE_VALUEB6layer_with_weights-1/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-1/bias/.ATTRIBUTES/VARIABLE_VALUEB6layer_with_weights-2/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-2/bias/.ATTRIBUTES/VARIABLE_VALUEB6layer_with_weights-3/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-3/bias/.ATTRIBUTES/VARIABLE_VALUEB6layer_with_weights-4/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-4/bias/.ATTRIBUTES/VARIABLE_VALUEB6layer_with_weights-5/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-5/bias/.ATTRIBUTES/VARIABLE_VALUEB6layer_with_weights-6/kernel/.ATTRIBUTES/VARIABLE_VALUEB4layer_with_weights-6/bias/.ATTRIBUTES/VARIABLE_VALUEB+optimizer/beta_1/.ATTRIBUTES/VARIABLE_VALUEB+optimizer/beta_2/.ATTRIBUTES/VARIABLE_VALUEB*optimizer/decay/.ATTRIBUTES/VARIABLE_VALUEB2optimizer/learning_rate/.ATTRIBUTES/VARIABLE_VALUEB)optimizer/iter/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/total/.ATTRIBUTES/VARIABLE_VALUEB4keras_api/metrics/0/count/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-0/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-0/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-1/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-1/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-2/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-2/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-3/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-3/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-4/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-4/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-5/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-5/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-6/kernel/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-6/bias/.OPTIMIZER_SLOT/optimizer/m/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-0/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-0/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-1/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-1/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-2/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-2/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-3/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-3/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-4/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-4/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-5/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-5/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBRlayer_with_weights-6/kernel/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEBPlayer_with_weights-6/bias/.OPTIMIZER_SLOT/optimizer/v/.ATTRIBUTES/VARIABLE_VALUEB_CHECKPOINTABLE_OBJECT_GRAPHÔ
RestoreV2/shape_and_slicesConst"/device:CPU:0*
_output_shapes
:2*
dtype0*w
valuenBl2B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B B 
	RestoreV2	RestoreV2file_prefixRestoreV2/tensor_names:output:0#RestoreV2/shape_and_slices:output:0"/device:CPU:0*Þ
_output_shapesË
È::::::::::::::::::::::::::::::::::::::::::::::::::*@
dtypes6
422	[
IdentityIdentityRestoreV2:tensors:0"/device:CPU:0*
T0*
_output_shapes
:
AssignVariableOpAssignVariableOp(assignvariableop_encoding_layer_1_kernelIdentity:output:0"/device:CPU:0*
_output_shapes
 *
dtype0]

Identity_1IdentityRestoreV2:tensors:1"/device:CPU:0*
T0*
_output_shapes
:
AssignVariableOp_1AssignVariableOp(assignvariableop_1_encoding_layer_1_biasIdentity_1:output:0"/device:CPU:0*
_output_shapes
 *
dtype0]

Identity_2IdentityRestoreV2:tensors:2"/device:CPU:0*
T0*
_output_shapes
:
AssignVariableOp_2AssignVariableOp*assignvariableop_2_encoding_layer_2_kernelIdentity_2:output:0"/device:CPU:0*
_output_shapes
 *
dtype0]

Identity_3IdentityRestoreV2:tensors:3"/device:CPU:0*
T0*
_output_shapes
:
AssignVariableOp_3AssignVariableOp(assignvariableop_3_encoding_layer_2_biasIdentity_3:output:0"/device:CPU:0*
_output_shapes
 *
dtype0]

Identity_4IdentityRestoreV2:tensors:4"/device:CPU:0*
T0*
_output_shapes
:
AssignVariableOp_4AssignVariableOp*assignvariableop_4_encoding_layer_3_kernelIdentity_4:output:0"/device:CPU:0*
_output_shapes
 *
dtype0]

Identity_5IdentityRestoreV2:tensors:5"/device:CPU:0*
T0*
_output_shapes
:
AssignVariableOp_5AssignVariableOp(assignvariableop_5_encoding_layer_3_biasIdentity_5:output:0"/device:CPU:0*
_output_shapes
 *
dtype0]

Identity_6IdentityRestoreV2:tensors:6"/device:CPU:0*
T0*
_output_shapes
:
AssignVariableOp_6AssignVariableOp*assignvariableop_6_encoding_layer_4_kernelIdentity_6:output:0"/device:CPU:0*
_output_shapes
 *
dtype0]

Identity_7IdentityRestoreV2:tensors:7"/device:CPU:0*
T0*
_output_shapes
:
AssignVariableOp_7AssignVariableOp(assignvariableop_7_encoding_layer_4_biasIdentity_7:output:0"/device:CPU:0*
_output_shapes
 *
dtype0]

Identity_8IdentityRestoreV2:tensors:8"/device:CPU:0*
T0*
_output_shapes
:
AssignVariableOp_8AssignVariableOp assignvariableop_8_z_mean_kernelIdentity_8:output:0"/device:CPU:0*
_output_shapes
 *
dtype0]

Identity_9IdentityRestoreV2:tensors:9"/device:CPU:0*
T0*
_output_shapes
:
AssignVariableOp_9AssignVariableOpassignvariableop_9_z_mean_biasIdentity_9:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_10IdentityRestoreV2:tensors:10"/device:CPU:0*
T0*
_output_shapes
:
AssignVariableOp_10AssignVariableOp&assignvariableop_10_z_log_sigma_kernelIdentity_10:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_11IdentityRestoreV2:tensors:11"/device:CPU:0*
T0*
_output_shapes
:
AssignVariableOp_11AssignVariableOp$assignvariableop_11_z_log_sigma_biasIdentity_11:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_12IdentityRestoreV2:tensors:12"/device:CPU:0*
T0*
_output_shapes
: 
AssignVariableOp_12AssignVariableOp/assignvariableop_12_classification_layer_kernelIdentity_12:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_13IdentityRestoreV2:tensors:13"/device:CPU:0*
T0*
_output_shapes
:
AssignVariableOp_13AssignVariableOp-assignvariableop_13_classification_layer_biasIdentity_13:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_14IdentityRestoreV2:tensors:14"/device:CPU:0*
T0*
_output_shapes
:
AssignVariableOp_14AssignVariableOpassignvariableop_14_beta_1Identity_14:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_15IdentityRestoreV2:tensors:15"/device:CPU:0*
T0*
_output_shapes
:
AssignVariableOp_15AssignVariableOpassignvariableop_15_beta_2Identity_15:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_16IdentityRestoreV2:tensors:16"/device:CPU:0*
T0*
_output_shapes
:
AssignVariableOp_16AssignVariableOpassignvariableop_16_decayIdentity_16:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_17IdentityRestoreV2:tensors:17"/device:CPU:0*
T0*
_output_shapes
:
AssignVariableOp_17AssignVariableOp!assignvariableop_17_learning_rateIdentity_17:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_18IdentityRestoreV2:tensors:18"/device:CPU:0*
T0	*
_output_shapes
:
AssignVariableOp_18AssignVariableOpassignvariableop_18_adam_iterIdentity_18:output:0"/device:CPU:0*
_output_shapes
 *
dtype0	_
Identity_19IdentityRestoreV2:tensors:19"/device:CPU:0*
T0*
_output_shapes
:
AssignVariableOp_19AssignVariableOpassignvariableop_19_totalIdentity_19:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_20IdentityRestoreV2:tensors:20"/device:CPU:0*
T0*
_output_shapes
:
AssignVariableOp_20AssignVariableOpassignvariableop_20_countIdentity_20:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_21IdentityRestoreV2:tensors:21"/device:CPU:0*
T0*
_output_shapes
:£
AssignVariableOp_21AssignVariableOp2assignvariableop_21_adam_encoding_layer_1_kernel_mIdentity_21:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_22IdentityRestoreV2:tensors:22"/device:CPU:0*
T0*
_output_shapes
:¡
AssignVariableOp_22AssignVariableOp0assignvariableop_22_adam_encoding_layer_1_bias_mIdentity_22:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_23IdentityRestoreV2:tensors:23"/device:CPU:0*
T0*
_output_shapes
:£
AssignVariableOp_23AssignVariableOp2assignvariableop_23_adam_encoding_layer_2_kernel_mIdentity_23:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_24IdentityRestoreV2:tensors:24"/device:CPU:0*
T0*
_output_shapes
:¡
AssignVariableOp_24AssignVariableOp0assignvariableop_24_adam_encoding_layer_2_bias_mIdentity_24:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_25IdentityRestoreV2:tensors:25"/device:CPU:0*
T0*
_output_shapes
:£
AssignVariableOp_25AssignVariableOp2assignvariableop_25_adam_encoding_layer_3_kernel_mIdentity_25:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_26IdentityRestoreV2:tensors:26"/device:CPU:0*
T0*
_output_shapes
:¡
AssignVariableOp_26AssignVariableOp0assignvariableop_26_adam_encoding_layer_3_bias_mIdentity_26:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_27IdentityRestoreV2:tensors:27"/device:CPU:0*
T0*
_output_shapes
:£
AssignVariableOp_27AssignVariableOp2assignvariableop_27_adam_encoding_layer_4_kernel_mIdentity_27:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_28IdentityRestoreV2:tensors:28"/device:CPU:0*
T0*
_output_shapes
:¡
AssignVariableOp_28AssignVariableOp0assignvariableop_28_adam_encoding_layer_4_bias_mIdentity_28:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_29IdentityRestoreV2:tensors:29"/device:CPU:0*
T0*
_output_shapes
:
AssignVariableOp_29AssignVariableOp(assignvariableop_29_adam_z_mean_kernel_mIdentity_29:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_30IdentityRestoreV2:tensors:30"/device:CPU:0*
T0*
_output_shapes
:
AssignVariableOp_30AssignVariableOp&assignvariableop_30_adam_z_mean_bias_mIdentity_30:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_31IdentityRestoreV2:tensors:31"/device:CPU:0*
T0*
_output_shapes
:
AssignVariableOp_31AssignVariableOp-assignvariableop_31_adam_z_log_sigma_kernel_mIdentity_31:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_32IdentityRestoreV2:tensors:32"/device:CPU:0*
T0*
_output_shapes
:
AssignVariableOp_32AssignVariableOp+assignvariableop_32_adam_z_log_sigma_bias_mIdentity_32:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_33IdentityRestoreV2:tensors:33"/device:CPU:0*
T0*
_output_shapes
:§
AssignVariableOp_33AssignVariableOp6assignvariableop_33_adam_classification_layer_kernel_mIdentity_33:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_34IdentityRestoreV2:tensors:34"/device:CPU:0*
T0*
_output_shapes
:¥
AssignVariableOp_34AssignVariableOp4assignvariableop_34_adam_classification_layer_bias_mIdentity_34:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_35IdentityRestoreV2:tensors:35"/device:CPU:0*
T0*
_output_shapes
:£
AssignVariableOp_35AssignVariableOp2assignvariableop_35_adam_encoding_layer_1_kernel_vIdentity_35:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_36IdentityRestoreV2:tensors:36"/device:CPU:0*
T0*
_output_shapes
:¡
AssignVariableOp_36AssignVariableOp0assignvariableop_36_adam_encoding_layer_1_bias_vIdentity_36:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_37IdentityRestoreV2:tensors:37"/device:CPU:0*
T0*
_output_shapes
:£
AssignVariableOp_37AssignVariableOp2assignvariableop_37_adam_encoding_layer_2_kernel_vIdentity_37:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_38IdentityRestoreV2:tensors:38"/device:CPU:0*
T0*
_output_shapes
:¡
AssignVariableOp_38AssignVariableOp0assignvariableop_38_adam_encoding_layer_2_bias_vIdentity_38:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_39IdentityRestoreV2:tensors:39"/device:CPU:0*
T0*
_output_shapes
:£
AssignVariableOp_39AssignVariableOp2assignvariableop_39_adam_encoding_layer_3_kernel_vIdentity_39:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_40IdentityRestoreV2:tensors:40"/device:CPU:0*
T0*
_output_shapes
:¡
AssignVariableOp_40AssignVariableOp0assignvariableop_40_adam_encoding_layer_3_bias_vIdentity_40:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_41IdentityRestoreV2:tensors:41"/device:CPU:0*
T0*
_output_shapes
:£
AssignVariableOp_41AssignVariableOp2assignvariableop_41_adam_encoding_layer_4_kernel_vIdentity_41:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_42IdentityRestoreV2:tensors:42"/device:CPU:0*
T0*
_output_shapes
:¡
AssignVariableOp_42AssignVariableOp0assignvariableop_42_adam_encoding_layer_4_bias_vIdentity_42:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_43IdentityRestoreV2:tensors:43"/device:CPU:0*
T0*
_output_shapes
:
AssignVariableOp_43AssignVariableOp(assignvariableop_43_adam_z_mean_kernel_vIdentity_43:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_44IdentityRestoreV2:tensors:44"/device:CPU:0*
T0*
_output_shapes
:
AssignVariableOp_44AssignVariableOp&assignvariableop_44_adam_z_mean_bias_vIdentity_44:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_45IdentityRestoreV2:tensors:45"/device:CPU:0*
T0*
_output_shapes
:
AssignVariableOp_45AssignVariableOp-assignvariableop_45_adam_z_log_sigma_kernel_vIdentity_45:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_46IdentityRestoreV2:tensors:46"/device:CPU:0*
T0*
_output_shapes
:
AssignVariableOp_46AssignVariableOp+assignvariableop_46_adam_z_log_sigma_bias_vIdentity_46:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_47IdentityRestoreV2:tensors:47"/device:CPU:0*
T0*
_output_shapes
:§
AssignVariableOp_47AssignVariableOp6assignvariableop_47_adam_classification_layer_kernel_vIdentity_47:output:0"/device:CPU:0*
_output_shapes
 *
dtype0_
Identity_48IdentityRestoreV2:tensors:48"/device:CPU:0*
T0*
_output_shapes
:¥
AssignVariableOp_48AssignVariableOp4assignvariableop_48_adam_classification_layer_bias_vIdentity_48:output:0"/device:CPU:0*
_output_shapes
 *
dtype01
NoOpNoOp"/device:CPU:0*
_output_shapes
 	
Identity_49Identityfile_prefix^AssignVariableOp^AssignVariableOp_1^AssignVariableOp_10^AssignVariableOp_11^AssignVariableOp_12^AssignVariableOp_13^AssignVariableOp_14^AssignVariableOp_15^AssignVariableOp_16^AssignVariableOp_17^AssignVariableOp_18^AssignVariableOp_19^AssignVariableOp_2^AssignVariableOp_20^AssignVariableOp_21^AssignVariableOp_22^AssignVariableOp_23^AssignVariableOp_24^AssignVariableOp_25^AssignVariableOp_26^AssignVariableOp_27^AssignVariableOp_28^AssignVariableOp_29^AssignVariableOp_3^AssignVariableOp_30^AssignVariableOp_31^AssignVariableOp_32^AssignVariableOp_33^AssignVariableOp_34^AssignVariableOp_35^AssignVariableOp_36^AssignVariableOp_37^AssignVariableOp_38^AssignVariableOp_39^AssignVariableOp_4^AssignVariableOp_40^AssignVariableOp_41^AssignVariableOp_42^AssignVariableOp_43^AssignVariableOp_44^AssignVariableOp_45^AssignVariableOp_46^AssignVariableOp_47^AssignVariableOp_48^AssignVariableOp_5^AssignVariableOp_6^AssignVariableOp_7^AssignVariableOp_8^AssignVariableOp_9^NoOp"/device:CPU:0*
T0*
_output_shapes
: W
Identity_50IdentityIdentity_49:output:0^NoOp_1*
T0*
_output_shapes
: ò
NoOp_1NoOp^AssignVariableOp^AssignVariableOp_1^AssignVariableOp_10^AssignVariableOp_11^AssignVariableOp_12^AssignVariableOp_13^AssignVariableOp_14^AssignVariableOp_15^AssignVariableOp_16^AssignVariableOp_17^AssignVariableOp_18^AssignVariableOp_19^AssignVariableOp_2^AssignVariableOp_20^AssignVariableOp_21^AssignVariableOp_22^AssignVariableOp_23^AssignVariableOp_24^AssignVariableOp_25^AssignVariableOp_26^AssignVariableOp_27^AssignVariableOp_28^AssignVariableOp_29^AssignVariableOp_3^AssignVariableOp_30^AssignVariableOp_31^AssignVariableOp_32^AssignVariableOp_33^AssignVariableOp_34^AssignVariableOp_35^AssignVariableOp_36^AssignVariableOp_37^AssignVariableOp_38^AssignVariableOp_39^AssignVariableOp_4^AssignVariableOp_40^AssignVariableOp_41^AssignVariableOp_42^AssignVariableOp_43^AssignVariableOp_44^AssignVariableOp_45^AssignVariableOp_46^AssignVariableOp_47^AssignVariableOp_48^AssignVariableOp_5^AssignVariableOp_6^AssignVariableOp_7^AssignVariableOp_8^AssignVariableOp_9*"
_acd_function_control_output(*
_output_shapes
 "#
identity_50Identity_50:output:0*w
_input_shapesf
d: : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : : 2$
AssignVariableOpAssignVariableOp2(
AssignVariableOp_1AssignVariableOp_12*
AssignVariableOp_10AssignVariableOp_102*
AssignVariableOp_11AssignVariableOp_112*
AssignVariableOp_12AssignVariableOp_122*
AssignVariableOp_13AssignVariableOp_132*
AssignVariableOp_14AssignVariableOp_142*
AssignVariableOp_15AssignVariableOp_152*
AssignVariableOp_16AssignVariableOp_162*
AssignVariableOp_17AssignVariableOp_172*
AssignVariableOp_18AssignVariableOp_182*
AssignVariableOp_19AssignVariableOp_192(
AssignVariableOp_2AssignVariableOp_22*
AssignVariableOp_20AssignVariableOp_202*
AssignVariableOp_21AssignVariableOp_212*
AssignVariableOp_22AssignVariableOp_222*
AssignVariableOp_23AssignVariableOp_232*
AssignVariableOp_24AssignVariableOp_242*
AssignVariableOp_25AssignVariableOp_252*
AssignVariableOp_26AssignVariableOp_262*
AssignVariableOp_27AssignVariableOp_272*
AssignVariableOp_28AssignVariableOp_282*
AssignVariableOp_29AssignVariableOp_292(
AssignVariableOp_3AssignVariableOp_32*
AssignVariableOp_30AssignVariableOp_302*
AssignVariableOp_31AssignVariableOp_312*
AssignVariableOp_32AssignVariableOp_322*
AssignVariableOp_33AssignVariableOp_332*
AssignVariableOp_34AssignVariableOp_342*
AssignVariableOp_35AssignVariableOp_352*
AssignVariableOp_36AssignVariableOp_362*
AssignVariableOp_37AssignVariableOp_372*
AssignVariableOp_38AssignVariableOp_382*
AssignVariableOp_39AssignVariableOp_392(
AssignVariableOp_4AssignVariableOp_42*
AssignVariableOp_40AssignVariableOp_402*
AssignVariableOp_41AssignVariableOp_412*
AssignVariableOp_42AssignVariableOp_422*
AssignVariableOp_43AssignVariableOp_432*
AssignVariableOp_44AssignVariableOp_442*
AssignVariableOp_45AssignVariableOp_452*
AssignVariableOp_46AssignVariableOp_462*
AssignVariableOp_47AssignVariableOp_472*
AssignVariableOp_48AssignVariableOp_482(
AssignVariableOp_5AssignVariableOp_52(
AssignVariableOp_6AssignVariableOp_62(
AssignVariableOp_7AssignVariableOp_72(
AssignVariableOp_8AssignVariableOp_82(
AssignVariableOp_9AssignVariableOp_9:C ?

_output_shapes
: 
%
_user_specified_namefile_prefix
¬


P__inference_classification_layer_layer_call_and_return_conditional_losses_459749

inputs0
matmul_readvariableop_resource: -
biasadd_readvariableop_resource:
identity¢BiasAdd/ReadVariableOp¢MatMul/ReadVariableOpt
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

: *
dtype0i
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿr
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:*
dtype0v
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿV
SoftmaxSoftmaxBiasAdd:output:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ`
IdentityIdentitySoftmax:softmax:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿw
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:ÿÿÿÿÿÿÿÿÿ : : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:O K
'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ 
 
_user_specified_nameinputs
Á

'__inference_z_mean_layer_call_fn_459642

inputs
unknown: 
	unknown_0:
identity¢StatefulPartitionedCallÚ
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ*$
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8 *K
fFRD
B__inference_z_mean_layer_call_and_return_conditional_losses_458827o
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:ÿÿÿÿÿÿÿÿÿ : : 22
StatefulPartitionedCallStatefulPartitionedCall:O K
'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ 
 
_user_specified_nameinputs
Ê	
ø
G__inference_z_log_sigma_layer_call_and_return_conditional_losses_458843

inputs0
matmul_readvariableop_resource: -
biasadd_readvariableop_resource:
identity¢BiasAdd/ReadVariableOp¢MatMul/ReadVariableOpt
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

: *
dtype0i
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿr
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:*
dtype0v
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ_
IdentityIdentityBiasAdd:output:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿw
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:ÿÿÿÿÿÿÿÿÿ : : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:O K
'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ 
 
_user_specified_nameinputs
ý

,__inference_VAE-encoder_layer_call_fn_458932	
input
unknown:
ý
	unknown_0:	
	unknown_1:

	unknown_2:	
	unknown_3:	@
	unknown_4:@
	unknown_5:@ 
	unknown_6: 
	unknown_7: 
	unknown_8:
	unknown_9: 

unknown_10:

unknown_11: 

unknown_12:
identity

identity_1

identity_2

identity_3¢StatefulPartitionedCall¹
StatefulPartitionedCallStatefulPartitionedCallinputunknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6	unknown_7	unknown_8	unknown_9
unknown_10
unknown_11
unknown_12*
Tin
2*
Tout
2*
_collective_manager_ids
 *`
_output_shapesN
L:ÿÿÿÿÿÿÿÿÿ:ÿÿÿÿÿÿÿÿÿ:ÿÿÿÿÿÿÿÿÿ:ÿÿÿÿÿÿÿÿÿ*0
_read_only_resource_inputs
	
*0
config_proto 

CPU

GPU2*0J 8 *P
fKRI
G__inference_VAE-encoder_layer_call_and_return_conditional_losses_458895o
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿq

Identity_1Identity StatefulPartitionedCall:output:1^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿq

Identity_2Identity StatefulPartitionedCall:output:2^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿq

Identity_3Identity StatefulPartitionedCall:output:3^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0*(
_construction_contextkEagerRuntime*C
_input_shapes2
0:ÿÿÿÿÿÿÿÿÿý: : : : : : : : : : : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:O K
(
_output_shapes
:ÿÿÿÿÿÿÿÿÿý

_user_specified_nameInput
¬


P__inference_classification_layer_layer_call_and_return_conditional_losses_458860

inputs0
matmul_readvariableop_resource: -
biasadd_readvariableop_resource:
identity¢BiasAdd/ReadVariableOp¢MatMul/ReadVariableOpt
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

: *
dtype0i
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿr
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:*
dtype0v
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿV
SoftmaxSoftmaxBiasAdd:output:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ`
IdentityIdentitySoftmax:softmax:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿw
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:ÿÿÿÿÿÿÿÿÿ : : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:O K
'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ 
 
_user_specified_nameinputs
ô
q
D__inference_Sampling_layer_call_and_return_conditional_losses_458965

inputs
inputs_1
identity;
ShapeShapeinputs*
T0*
_output_shapes
:]
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: _
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:_
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:Ñ
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask_
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB:a
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:a
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:Ù
strided_slice_1StridedSliceShape:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask{
random_normal/shapePackstrided_slice:output:0strided_slice_1:output:0*
N*
T0*
_output_shapes
:W
random_normal/meanConst*
_output_shapes
: *
dtype0*
valueB
 *    Y
random_normal/stddevConst*
_output_shapes
: *
dtype0*
valueB
 *
×£;
"random_normal/RandomStandardNormalRandomStandardNormalrandom_normal/shape:output:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ*
dtype0
random_normal/mulMul+random_normal/RandomStandardNormal:output:0random_normal/stddev:output:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ|
random_normalAddV2random_normal/mul:z:0random_normal/mean:output:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿF
ExpExpinputs_1*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿX
mulMulExp:y:0random_normal:z:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿO
addAddV2inputsmul:z:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿO
IdentityIdentityadd:z:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:ÿÿÿÿÿÿÿÿÿ:ÿÿÿÿÿÿÿÿÿ:O K
'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
 
_user_specified_nameinputs:OK
'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
 
_user_specified_nameinputs
÷
r
)__inference_Sampling_layer_call_fn_459677
inputs_0
inputs_1
identity¢StatefulPartitionedCallÏ
StatefulPartitionedCallStatefulPartitionedCallinputs_0inputs_1*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ* 
_read_only_resource_inputs
 *0
config_proto 

CPU

GPU2*0J 8 *M
fHRF
D__inference_Sampling_layer_call_and_return_conditional_losses_458889o
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:ÿÿÿÿÿÿÿÿÿ:ÿÿÿÿÿÿÿÿÿ22
StatefulPartitionedCallStatefulPartitionedCall:Q M
'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
"
_user_specified_name
inputs/0:QM
'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
"
_user_specified_name
inputs/1
¥


L__inference_Encoding_layer_2_layer_call_and_return_conditional_losses_459593

inputs2
matmul_readvariableop_resource:
.
biasadd_readvariableop_resource:	
identity¢BiasAdd/ReadVariableOp¢MatMul/ReadVariableOpv
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource* 
_output_shapes
:
*
dtype0j
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:ÿÿÿÿÿÿÿÿÿs
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:*
dtype0w
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*(
_output_shapes
:ÿÿÿÿÿÿÿÿÿQ
TanhTanhBiasAdd:output:0*
T0*(
_output_shapes
:ÿÿÿÿÿÿÿÿÿX
IdentityIdentityTanh:y:0^NoOp*
T0*(
_output_shapes
:ÿÿÿÿÿÿÿÿÿw
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:ÿÿÿÿÿÿÿÿÿ: : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:P L
(
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
 
_user_specified_nameinputs
÷1

G__inference_VAE-encoder_layer_call_and_return_conditional_losses_459284	
input+
encoding_layer_1_459244:
ý&
encoding_layer_1_459246:	+
encoding_layer_2_459249:
&
encoding_layer_2_459251:	*
encoding_layer_3_459254:	@%
encoding_layer_3_459256:@)
encoding_layer_4_459259:@ %
encoding_layer_4_459261: 
z_mean_459264: 
z_mean_459266:$
z_log_sigma_459269:  
z_log_sigma_459271:-
classification_layer_459274: )
classification_layer_459276:
identity

identity_1

identity_2

identity_3¢(Encoding_layer_1/StatefulPartitionedCall¢(Encoding_layer_2/StatefulPartitionedCall¢(Encoding_layer_3/StatefulPartitionedCall¢(Encoding_layer_4/StatefulPartitionedCall¢ Sampling/StatefulPartitionedCall¢,classification_layer/StatefulPartitionedCall¢#z_log_sigma/StatefulPartitionedCall¢z_mean/StatefulPartitionedCall
(Encoding_layer_1/StatefulPartitionedCallStatefulPartitionedCallinputencoding_layer_1_459244encoding_layer_1_459246*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:ÿÿÿÿÿÿÿÿÿ*$
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8 *U
fPRN
L__inference_Encoding_layer_1_layer_call_and_return_conditional_losses_458760¿
(Encoding_layer_2/StatefulPartitionedCallStatefulPartitionedCall1Encoding_layer_1/StatefulPartitionedCall:output:0encoding_layer_2_459249encoding_layer_2_459251*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:ÿÿÿÿÿÿÿÿÿ*$
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8 *U
fPRN
L__inference_Encoding_layer_2_layer_call_and_return_conditional_losses_458777¾
(Encoding_layer_3/StatefulPartitionedCallStatefulPartitionedCall1Encoding_layer_2/StatefulPartitionedCall:output:0encoding_layer_3_459254encoding_layer_3_459256*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ@*$
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8 *U
fPRN
L__inference_Encoding_layer_3_layer_call_and_return_conditional_losses_458794¾
(Encoding_layer_4/StatefulPartitionedCallStatefulPartitionedCall1Encoding_layer_3/StatefulPartitionedCall:output:0encoding_layer_4_459259encoding_layer_4_459261*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ *$
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8 *U
fPRN
L__inference_Encoding_layer_4_layer_call_and_return_conditional_losses_458811
z_mean/StatefulPartitionedCallStatefulPartitionedCall1Encoding_layer_4/StatefulPartitionedCall:output:0z_mean_459264z_mean_459266*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ*$
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8 *K
fFRD
B__inference_z_mean_layer_call_and_return_conditional_losses_458827ª
#z_log_sigma/StatefulPartitionedCallStatefulPartitionedCall1Encoding_layer_4/StatefulPartitionedCall:output:0z_log_sigma_459269z_log_sigma_459271*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ*$
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8 *P
fKRI
G__inference_z_log_sigma_layer_call_and_return_conditional_losses_458843Î
,classification_layer/StatefulPartitionedCallStatefulPartitionedCall1Encoding_layer_4/StatefulPartitionedCall:output:0classification_layer_459274classification_layer_459276*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ*$
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8 *Y
fTRR
P__inference_classification_layer_layer_call_and_return_conditional_losses_458860
 Sampling/StatefulPartitionedCallStatefulPartitionedCall'z_mean/StatefulPartitionedCall:output:0,z_log_sigma/StatefulPartitionedCall:output:0*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ* 
_read_only_resource_inputs
 *0
config_proto 

CPU

GPU2*0J 8 *M
fHRF
D__inference_Sampling_layer_call_and_return_conditional_losses_458965v
IdentityIdentity'z_mean/StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ}

Identity_1Identity,z_log_sigma/StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿz

Identity_2Identity)Sampling/StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ

Identity_3Identity5classification_layer/StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
NoOpNoOp)^Encoding_layer_1/StatefulPartitionedCall)^Encoding_layer_2/StatefulPartitionedCall)^Encoding_layer_3/StatefulPartitionedCall)^Encoding_layer_4/StatefulPartitionedCall!^Sampling/StatefulPartitionedCall-^classification_layer/StatefulPartitionedCall$^z_log_sigma/StatefulPartitionedCall^z_mean/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0*(
_construction_contextkEagerRuntime*C
_input_shapes2
0:ÿÿÿÿÿÿÿÿÿý: : : : : : : : : : : : : : 2T
(Encoding_layer_1/StatefulPartitionedCall(Encoding_layer_1/StatefulPartitionedCall2T
(Encoding_layer_2/StatefulPartitionedCall(Encoding_layer_2/StatefulPartitionedCall2T
(Encoding_layer_3/StatefulPartitionedCall(Encoding_layer_3/StatefulPartitionedCall2T
(Encoding_layer_4/StatefulPartitionedCall(Encoding_layer_4/StatefulPartitionedCall2D
 Sampling/StatefulPartitionedCall Sampling/StatefulPartitionedCall2\
,classification_layer/StatefulPartitionedCall,classification_layer/StatefulPartitionedCall2J
#z_log_sigma/StatefulPartitionedCall#z_log_sigma/StatefulPartitionedCall2@
z_mean/StatefulPartitionedCallz_mean/StatefulPartitionedCall:O K
(
_output_shapes
:ÿÿÿÿÿÿÿÿÿý

_user_specified_nameInput
Ý
¢
5__inference_classification_layer_layer_call_fn_459738

inputs
unknown: 
	unknown_0:
identity¢StatefulPartitionedCallè
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ*$
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8 *Y
fTRR
P__inference_classification_layer_layer_call_and_return_conditional_losses_458860o
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:ÿÿÿÿÿÿÿÿÿ : : 22
StatefulPartitionedCallStatefulPartitionedCall:O K
'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ 
 
_user_specified_nameinputs
ô
q
D__inference_Sampling_layer_call_and_return_conditional_losses_458889

inputs
inputs_1
identity;
ShapeShapeinputs*
T0*
_output_shapes
:]
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: _
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:_
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:Ñ
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask_
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB:a
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:a
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:Ù
strided_slice_1StridedSliceShape:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask{
random_normal/shapePackstrided_slice:output:0strided_slice_1:output:0*
N*
T0*
_output_shapes
:W
random_normal/meanConst*
_output_shapes
: *
dtype0*
valueB
 *    Y
random_normal/stddevConst*
_output_shapes
: *
dtype0*
valueB
 *
×£;
"random_normal/RandomStandardNormalRandomStandardNormalrandom_normal/shape:output:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ*
dtype0
random_normal/mulMul+random_normal/RandomStandardNormal:output:0random_normal/stddev:output:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ|
random_normalAddV2random_normal/mul:z:0random_normal/mean:output:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿF
ExpExpinputs_1*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿX
mulMulExp:y:0random_normal:z:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿO
addAddV2inputsmul:z:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿO
IdentityIdentityadd:z:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:ÿÿÿÿÿÿÿÿÿ:ÿÿÿÿÿÿÿÿÿ:O K
'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
 
_user_specified_nameinputs:OK
'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
 
_user_specified_nameinputs
þ
s
D__inference_Sampling_layer_call_and_return_conditional_losses_459706
inputs_0
inputs_1
identity=
ShapeShapeinputs_0*
T0*
_output_shapes
:]
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: _
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:_
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:Ñ
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask_
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB:a
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:a
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:Ù
strided_slice_1StridedSliceShape:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask{
random_normal/shapePackstrided_slice:output:0strided_slice_1:output:0*
N*
T0*
_output_shapes
:W
random_normal/meanConst*
_output_shapes
: *
dtype0*
valueB
 *    Y
random_normal/stddevConst*
_output_shapes
: *
dtype0*
valueB
 *
×£;
"random_normal/RandomStandardNormalRandomStandardNormalrandom_normal/shape:output:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ*
dtype0
random_normal/mulMul+random_normal/RandomStandardNormal:output:0random_normal/stddev:output:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ|
random_normalAddV2random_normal/mul:z:0random_normal/mean:output:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿF
ExpExpinputs_1*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿX
mulMulExp:y:0random_normal:z:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿQ
addAddV2inputs_0mul:z:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿO
IdentityIdentityadd:z:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:ÿÿÿÿÿÿÿÿÿ:ÿÿÿÿÿÿÿÿÿ:Q M
'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
"
_user_specified_name
inputs/0:QM
'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
"
_user_specified_name
inputs/1
Å	
ó
B__inference_z_mean_layer_call_and_return_conditional_losses_459652

inputs0
matmul_readvariableop_resource: -
biasadd_readvariableop_resource:
identity¢BiasAdd/ReadVariableOp¢MatMul/ReadVariableOpt
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

: *
dtype0i
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿr
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:*
dtype0v
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ_
IdentityIdentityBiasAdd:output:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿw
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:ÿÿÿÿÿÿÿÿÿ : : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:O K
'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ 
 
_user_specified_nameinputs
Ø

1__inference_Encoding_layer_3_layer_call_fn_459602

inputs
unknown:	@
	unknown_0:@
identity¢StatefulPartitionedCallä
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ@*$
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8 *U
fPRN
L__inference_Encoding_layer_3_layer_call_and_return_conditional_losses_458794o
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ@`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:ÿÿÿÿÿÿÿÿÿ: : 22
StatefulPartitionedCallStatefulPartitionedCall:P L
(
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
 
_user_specified_nameinputs


þ
L__inference_Encoding_layer_3_layer_call_and_return_conditional_losses_459613

inputs1
matmul_readvariableop_resource:	@-
biasadd_readvariableop_resource:@
identity¢BiasAdd/ReadVariableOp¢MatMul/ReadVariableOpu
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	@*
dtype0i
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ@r
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:@*
dtype0v
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ@P
TanhTanhBiasAdd:output:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ@W
IdentityIdentityTanh:y:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ@w
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:ÿÿÿÿÿÿÿÿÿ: : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:P L
(
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
 
_user_specified_nameinputs
Å	
ó
B__inference_z_mean_layer_call_and_return_conditional_losses_458827

inputs0
matmul_readvariableop_resource: -
biasadd_readvariableop_resource:
identity¢BiasAdd/ReadVariableOp¢MatMul/ReadVariableOpt
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

: *
dtype0i
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿr
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:*
dtype0v
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ_
IdentityIdentityBiasAdd:output:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿw
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:ÿÿÿÿÿÿÿÿÿ : : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:O K
'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ 
 
_user_specified_nameinputs


,__inference_VAE-encoder_layer_call_fn_459368

inputs
unknown:
ý
	unknown_0:	
	unknown_1:

	unknown_2:	
	unknown_3:	@
	unknown_4:@
	unknown_5:@ 
	unknown_6: 
	unknown_7: 
	unknown_8:
	unknown_9: 

unknown_10:

unknown_11: 

unknown_12:
identity

identity_1

identity_2

identity_3¢StatefulPartitionedCallº
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6	unknown_7	unknown_8	unknown_9
unknown_10
unknown_11
unknown_12*
Tin
2*
Tout
2*
_collective_manager_ids
 *`
_output_shapesN
L:ÿÿÿÿÿÿÿÿÿ:ÿÿÿÿÿÿÿÿÿ:ÿÿÿÿÿÿÿÿÿ:ÿÿÿÿÿÿÿÿÿ*0
_read_only_resource_inputs
	
*0
config_proto 

CPU

GPU2*0J 8 *P
fKRI
G__inference_VAE-encoder_layer_call_and_return_conditional_losses_459122o
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿq

Identity_1Identity StatefulPartitionedCall:output:1^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿq

Identity_2Identity StatefulPartitionedCall:output:2^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿq

Identity_3Identity StatefulPartitionedCall:output:3^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0*(
_construction_contextkEagerRuntime*C
_input_shapes2
0:ÿÿÿÿÿÿÿÿÿý: : : : : : : : : : : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:P L
(
_output_shapes
:ÿÿÿÿÿÿÿÿÿý
 
_user_specified_nameinputs
Õ

1__inference_Encoding_layer_4_layer_call_fn_459622

inputs
unknown:@ 
	unknown_0: 
identity¢StatefulPartitionedCallä
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ *$
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8 *U
fPRN
L__inference_Encoding_layer_4_layer_call_and_return_conditional_losses_458811o
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ `
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:ÿÿÿÿÿÿÿÿÿ@: : 22
StatefulPartitionedCallStatefulPartitionedCall:O K
'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ@
 
_user_specified_nameinputs
Ùk
ô
!__inference__wrapped_model_458742	
inputO
;vae_encoder_encoding_layer_1_matmul_readvariableop_resource:
ýK
<vae_encoder_encoding_layer_1_biasadd_readvariableop_resource:	O
;vae_encoder_encoding_layer_2_matmul_readvariableop_resource:
K
<vae_encoder_encoding_layer_2_biasadd_readvariableop_resource:	N
;vae_encoder_encoding_layer_3_matmul_readvariableop_resource:	@J
<vae_encoder_encoding_layer_3_biasadd_readvariableop_resource:@M
;vae_encoder_encoding_layer_4_matmul_readvariableop_resource:@ J
<vae_encoder_encoding_layer_4_biasadd_readvariableop_resource: C
1vae_encoder_z_mean_matmul_readvariableop_resource: @
2vae_encoder_z_mean_biasadd_readvariableop_resource:H
6vae_encoder_z_log_sigma_matmul_readvariableop_resource: E
7vae_encoder_z_log_sigma_biasadd_readvariableop_resource:Q
?vae_encoder_classification_layer_matmul_readvariableop_resource: N
@vae_encoder_classification_layer_biasadd_readvariableop_resource:
identity

identity_1

identity_2

identity_3¢3VAE-encoder/Encoding_layer_1/BiasAdd/ReadVariableOp¢2VAE-encoder/Encoding_layer_1/MatMul/ReadVariableOp¢3VAE-encoder/Encoding_layer_2/BiasAdd/ReadVariableOp¢2VAE-encoder/Encoding_layer_2/MatMul/ReadVariableOp¢3VAE-encoder/Encoding_layer_3/BiasAdd/ReadVariableOp¢2VAE-encoder/Encoding_layer_3/MatMul/ReadVariableOp¢3VAE-encoder/Encoding_layer_4/BiasAdd/ReadVariableOp¢2VAE-encoder/Encoding_layer_4/MatMul/ReadVariableOp¢7VAE-encoder/classification_layer/BiasAdd/ReadVariableOp¢6VAE-encoder/classification_layer/MatMul/ReadVariableOp¢.VAE-encoder/z_log_sigma/BiasAdd/ReadVariableOp¢-VAE-encoder/z_log_sigma/MatMul/ReadVariableOp¢)VAE-encoder/z_mean/BiasAdd/ReadVariableOp¢(VAE-encoder/z_mean/MatMul/ReadVariableOp°
2VAE-encoder/Encoding_layer_1/MatMul/ReadVariableOpReadVariableOp;vae_encoder_encoding_layer_1_matmul_readvariableop_resource* 
_output_shapes
:
ý*
dtype0£
#VAE-encoder/Encoding_layer_1/MatMulMatMulinput:VAE-encoder/Encoding_layer_1/MatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:ÿÿÿÿÿÿÿÿÿ­
3VAE-encoder/Encoding_layer_1/BiasAdd/ReadVariableOpReadVariableOp<vae_encoder_encoding_layer_1_biasadd_readvariableop_resource*
_output_shapes	
:*
dtype0Î
$VAE-encoder/Encoding_layer_1/BiasAddBiasAdd-VAE-encoder/Encoding_layer_1/MatMul:product:0;VAE-encoder/Encoding_layer_1/BiasAdd/ReadVariableOp:value:0*
T0*(
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
!VAE-encoder/Encoding_layer_1/TanhTanh-VAE-encoder/Encoding_layer_1/BiasAdd:output:0*
T0*(
_output_shapes
:ÿÿÿÿÿÿÿÿÿ°
2VAE-encoder/Encoding_layer_2/MatMul/ReadVariableOpReadVariableOp;vae_encoder_encoding_layer_2_matmul_readvariableop_resource* 
_output_shapes
:
*
dtype0Ã
#VAE-encoder/Encoding_layer_2/MatMulMatMul%VAE-encoder/Encoding_layer_1/Tanh:y:0:VAE-encoder/Encoding_layer_2/MatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:ÿÿÿÿÿÿÿÿÿ­
3VAE-encoder/Encoding_layer_2/BiasAdd/ReadVariableOpReadVariableOp<vae_encoder_encoding_layer_2_biasadd_readvariableop_resource*
_output_shapes	
:*
dtype0Î
$VAE-encoder/Encoding_layer_2/BiasAddBiasAdd-VAE-encoder/Encoding_layer_2/MatMul:product:0;VAE-encoder/Encoding_layer_2/BiasAdd/ReadVariableOp:value:0*
T0*(
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
!VAE-encoder/Encoding_layer_2/TanhTanh-VAE-encoder/Encoding_layer_2/BiasAdd:output:0*
T0*(
_output_shapes
:ÿÿÿÿÿÿÿÿÿ¯
2VAE-encoder/Encoding_layer_3/MatMul/ReadVariableOpReadVariableOp;vae_encoder_encoding_layer_3_matmul_readvariableop_resource*
_output_shapes
:	@*
dtype0Â
#VAE-encoder/Encoding_layer_3/MatMulMatMul%VAE-encoder/Encoding_layer_2/Tanh:y:0:VAE-encoder/Encoding_layer_3/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ@¬
3VAE-encoder/Encoding_layer_3/BiasAdd/ReadVariableOpReadVariableOp<vae_encoder_encoding_layer_3_biasadd_readvariableop_resource*
_output_shapes
:@*
dtype0Í
$VAE-encoder/Encoding_layer_3/BiasAddBiasAdd-VAE-encoder/Encoding_layer_3/MatMul:product:0;VAE-encoder/Encoding_layer_3/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ@
!VAE-encoder/Encoding_layer_3/TanhTanh-VAE-encoder/Encoding_layer_3/BiasAdd:output:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ@®
2VAE-encoder/Encoding_layer_4/MatMul/ReadVariableOpReadVariableOp;vae_encoder_encoding_layer_4_matmul_readvariableop_resource*
_output_shapes

:@ *
dtype0Â
#VAE-encoder/Encoding_layer_4/MatMulMatMul%VAE-encoder/Encoding_layer_3/Tanh:y:0:VAE-encoder/Encoding_layer_4/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ ¬
3VAE-encoder/Encoding_layer_4/BiasAdd/ReadVariableOpReadVariableOp<vae_encoder_encoding_layer_4_biasadd_readvariableop_resource*
_output_shapes
: *
dtype0Í
$VAE-encoder/Encoding_layer_4/BiasAddBiasAdd-VAE-encoder/Encoding_layer_4/MatMul:product:0;VAE-encoder/Encoding_layer_4/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ 
!VAE-encoder/Encoding_layer_4/TanhTanh-VAE-encoder/Encoding_layer_4/BiasAdd:output:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ 
(VAE-encoder/z_mean/MatMul/ReadVariableOpReadVariableOp1vae_encoder_z_mean_matmul_readvariableop_resource*
_output_shapes

: *
dtype0®
VAE-encoder/z_mean/MatMulMatMul%VAE-encoder/Encoding_layer_4/Tanh:y:00VAE-encoder/z_mean/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
)VAE-encoder/z_mean/BiasAdd/ReadVariableOpReadVariableOp2vae_encoder_z_mean_biasadd_readvariableop_resource*
_output_shapes
:*
dtype0¯
VAE-encoder/z_mean/BiasAddBiasAdd#VAE-encoder/z_mean/MatMul:product:01VAE-encoder/z_mean/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ¤
-VAE-encoder/z_log_sigma/MatMul/ReadVariableOpReadVariableOp6vae_encoder_z_log_sigma_matmul_readvariableop_resource*
_output_shapes

: *
dtype0¸
VAE-encoder/z_log_sigma/MatMulMatMul%VAE-encoder/Encoding_layer_4/Tanh:y:05VAE-encoder/z_log_sigma/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ¢
.VAE-encoder/z_log_sigma/BiasAdd/ReadVariableOpReadVariableOp7vae_encoder_z_log_sigma_biasadd_readvariableop_resource*
_output_shapes
:*
dtype0¾
VAE-encoder/z_log_sigma/BiasAddBiasAdd(VAE-encoder/z_log_sigma/MatMul:product:06VAE-encoder/z_log_sigma/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ¶
6VAE-encoder/classification_layer/MatMul/ReadVariableOpReadVariableOp?vae_encoder_classification_layer_matmul_readvariableop_resource*
_output_shapes

: *
dtype0Ê
'VAE-encoder/classification_layer/MatMulMatMul%VAE-encoder/Encoding_layer_4/Tanh:y:0>VAE-encoder/classification_layer/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ´
7VAE-encoder/classification_layer/BiasAdd/ReadVariableOpReadVariableOp@vae_encoder_classification_layer_biasadd_readvariableop_resource*
_output_shapes
:*
dtype0Ù
(VAE-encoder/classification_layer/BiasAddBiasAdd1VAE-encoder/classification_layer/MatMul:product:0?VAE-encoder/classification_layer/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
(VAE-encoder/classification_layer/SoftmaxSoftmax1VAE-encoder/classification_layer/BiasAdd:output:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿm
VAE-encoder/Sampling/ShapeShape#VAE-encoder/z_mean/BiasAdd:output:0*
T0*
_output_shapes
:r
(VAE-encoder/Sampling/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: t
*VAE-encoder/Sampling/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:t
*VAE-encoder/Sampling/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:º
"VAE-encoder/Sampling/strided_sliceStridedSlice#VAE-encoder/Sampling/Shape:output:01VAE-encoder/Sampling/strided_slice/stack:output:03VAE-encoder/Sampling/strided_slice/stack_1:output:03VAE-encoder/Sampling/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_maskt
*VAE-encoder/Sampling/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB:v
,VAE-encoder/Sampling/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:v
,VAE-encoder/Sampling/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:Â
$VAE-encoder/Sampling/strided_slice_1StridedSlice#VAE-encoder/Sampling/Shape:output:03VAE-encoder/Sampling/strided_slice_1/stack:output:05VAE-encoder/Sampling/strided_slice_1/stack_1:output:05VAE-encoder/Sampling/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_maskº
(VAE-encoder/Sampling/random_normal/shapePack+VAE-encoder/Sampling/strided_slice:output:0-VAE-encoder/Sampling/strided_slice_1:output:0*
N*
T0*
_output_shapes
:l
'VAE-encoder/Sampling/random_normal/meanConst*
_output_shapes
: *
dtype0*
valueB
 *    n
)VAE-encoder/Sampling/random_normal/stddevConst*
_output_shapes
: *
dtype0*
valueB
 *
×£;Á
7VAE-encoder/Sampling/random_normal/RandomStandardNormalRandomStandardNormal1VAE-encoder/Sampling/random_normal/shape:output:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ*
dtype0Õ
&VAE-encoder/Sampling/random_normal/mulMul@VAE-encoder/Sampling/random_normal/RandomStandardNormal:output:02VAE-encoder/Sampling/random_normal/stddev:output:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ»
"VAE-encoder/Sampling/random_normalAddV2*VAE-encoder/Sampling/random_normal/mul:z:00VAE-encoder/Sampling/random_normal/mean:output:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ{
VAE-encoder/Sampling/ExpExp(VAE-encoder/z_log_sigma/BiasAdd:output:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
VAE-encoder/Sampling/mulMulVAE-encoder/Sampling/Exp:y:0&VAE-encoder/Sampling/random_normal:z:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
VAE-encoder/Sampling/addAddV2#VAE-encoder/z_mean/BiasAdd:output:0VAE-encoder/Sampling/mul:z:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿk
IdentityIdentityVAE-encoder/Sampling/add:z:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ

Identity_1Identity2VAE-encoder/classification_layer/Softmax:softmax:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿy

Identity_2Identity(VAE-encoder/z_log_sigma/BiasAdd:output:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿt

Identity_3Identity#VAE-encoder/z_mean/BiasAdd:output:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
NoOpNoOp4^VAE-encoder/Encoding_layer_1/BiasAdd/ReadVariableOp3^VAE-encoder/Encoding_layer_1/MatMul/ReadVariableOp4^VAE-encoder/Encoding_layer_2/BiasAdd/ReadVariableOp3^VAE-encoder/Encoding_layer_2/MatMul/ReadVariableOp4^VAE-encoder/Encoding_layer_3/BiasAdd/ReadVariableOp3^VAE-encoder/Encoding_layer_3/MatMul/ReadVariableOp4^VAE-encoder/Encoding_layer_4/BiasAdd/ReadVariableOp3^VAE-encoder/Encoding_layer_4/MatMul/ReadVariableOp8^VAE-encoder/classification_layer/BiasAdd/ReadVariableOp7^VAE-encoder/classification_layer/MatMul/ReadVariableOp/^VAE-encoder/z_log_sigma/BiasAdd/ReadVariableOp.^VAE-encoder/z_log_sigma/MatMul/ReadVariableOp*^VAE-encoder/z_mean/BiasAdd/ReadVariableOp)^VAE-encoder/z_mean/MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0*(
_construction_contextkEagerRuntime*C
_input_shapes2
0:ÿÿÿÿÿÿÿÿÿý: : : : : : : : : : : : : : 2j
3VAE-encoder/Encoding_layer_1/BiasAdd/ReadVariableOp3VAE-encoder/Encoding_layer_1/BiasAdd/ReadVariableOp2h
2VAE-encoder/Encoding_layer_1/MatMul/ReadVariableOp2VAE-encoder/Encoding_layer_1/MatMul/ReadVariableOp2j
3VAE-encoder/Encoding_layer_2/BiasAdd/ReadVariableOp3VAE-encoder/Encoding_layer_2/BiasAdd/ReadVariableOp2h
2VAE-encoder/Encoding_layer_2/MatMul/ReadVariableOp2VAE-encoder/Encoding_layer_2/MatMul/ReadVariableOp2j
3VAE-encoder/Encoding_layer_3/BiasAdd/ReadVariableOp3VAE-encoder/Encoding_layer_3/BiasAdd/ReadVariableOp2h
2VAE-encoder/Encoding_layer_3/MatMul/ReadVariableOp2VAE-encoder/Encoding_layer_3/MatMul/ReadVariableOp2j
3VAE-encoder/Encoding_layer_4/BiasAdd/ReadVariableOp3VAE-encoder/Encoding_layer_4/BiasAdd/ReadVariableOp2h
2VAE-encoder/Encoding_layer_4/MatMul/ReadVariableOp2VAE-encoder/Encoding_layer_4/MatMul/ReadVariableOp2r
7VAE-encoder/classification_layer/BiasAdd/ReadVariableOp7VAE-encoder/classification_layer/BiasAdd/ReadVariableOp2p
6VAE-encoder/classification_layer/MatMul/ReadVariableOp6VAE-encoder/classification_layer/MatMul/ReadVariableOp2`
.VAE-encoder/z_log_sigma/BiasAdd/ReadVariableOp.VAE-encoder/z_log_sigma/BiasAdd/ReadVariableOp2^
-VAE-encoder/z_log_sigma/MatMul/ReadVariableOp-VAE-encoder/z_log_sigma/MatMul/ReadVariableOp2V
)VAE-encoder/z_mean/BiasAdd/ReadVariableOp)VAE-encoder/z_mean/BiasAdd/ReadVariableOp2T
(VAE-encoder/z_mean/MatMul/ReadVariableOp(VAE-encoder/z_mean/MatMul/ReadVariableOp:O K
(
_output_shapes
:ÿÿÿÿÿÿÿÿÿý

_user_specified_nameInput
¥


L__inference_Encoding_layer_2_layer_call_and_return_conditional_losses_458777

inputs2
matmul_readvariableop_resource:
.
biasadd_readvariableop_resource:	
identity¢BiasAdd/ReadVariableOp¢MatMul/ReadVariableOpv
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource* 
_output_shapes
:
*
dtype0j
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:ÿÿÿÿÿÿÿÿÿs
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:*
dtype0w
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*(
_output_shapes
:ÿÿÿÿÿÿÿÿÿQ
TanhTanhBiasAdd:output:0*
T0*(
_output_shapes
:ÿÿÿÿÿÿÿÿÿX
IdentityIdentityTanh:y:0^NoOp*
T0*(
_output_shapes
:ÿÿÿÿÿÿÿÿÿw
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:ÿÿÿÿÿÿÿÿÿ: : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:P L
(
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
 
_user_specified_nameinputs
¥


L__inference_Encoding_layer_1_layer_call_and_return_conditional_losses_458760

inputs2
matmul_readvariableop_resource:
ý.
biasadd_readvariableop_resource:	
identity¢BiasAdd/ReadVariableOp¢MatMul/ReadVariableOpv
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource* 
_output_shapes
:
ý*
dtype0j
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:ÿÿÿÿÿÿÿÿÿs
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:*
dtype0w
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*(
_output_shapes
:ÿÿÿÿÿÿÿÿÿQ
TanhTanhBiasAdd:output:0*
T0*(
_output_shapes
:ÿÿÿÿÿÿÿÿÿX
IdentityIdentityTanh:y:0^NoOp*
T0*(
_output_shapes
:ÿÿÿÿÿÿÿÿÿw
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:ÿÿÿÿÿÿÿÿÿý: : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:P L
(
_output_shapes
:ÿÿÿÿÿÿÿÿÿý
 
_user_specified_nameinputs
Ë

,__inference_z_log_sigma_layer_call_fn_459661

inputs
unknown: 
	unknown_0:
identity¢StatefulPartitionedCallß
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ*$
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8 *P
fKRI
G__inference_z_log_sigma_layer_call_and_return_conditional_losses_458843o
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:ÿÿÿÿÿÿÿÿÿ : : 22
StatefulPartitionedCallStatefulPartitionedCall:O K
'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ 
 
_user_specified_nameinputs
ý

,__inference_VAE-encoder_layer_call_fn_459198	
input
unknown:
ý
	unknown_0:	
	unknown_1:

	unknown_2:	
	unknown_3:	@
	unknown_4:@
	unknown_5:@ 
	unknown_6: 
	unknown_7: 
	unknown_8:
	unknown_9: 

unknown_10:

unknown_11: 

unknown_12:
identity

identity_1

identity_2

identity_3¢StatefulPartitionedCall¹
StatefulPartitionedCallStatefulPartitionedCallinputunknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6	unknown_7	unknown_8	unknown_9
unknown_10
unknown_11
unknown_12*
Tin
2*
Tout
2*
_collective_manager_ids
 *`
_output_shapesN
L:ÿÿÿÿÿÿÿÿÿ:ÿÿÿÿÿÿÿÿÿ:ÿÿÿÿÿÿÿÿÿ:ÿÿÿÿÿÿÿÿÿ*0
_read_only_resource_inputs
	
*0
config_proto 

CPU

GPU2*0J 8 *P
fKRI
G__inference_VAE-encoder_layer_call_and_return_conditional_losses_459122o
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿq

Identity_1Identity StatefulPartitionedCall:output:1^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿq

Identity_2Identity StatefulPartitionedCall:output:2^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿq

Identity_3Identity StatefulPartitionedCall:output:3^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0*(
_construction_contextkEagerRuntime*C
_input_shapes2
0:ÿÿÿÿÿÿÿÿÿý: : : : : : : : : : : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:O K
(
_output_shapes
:ÿÿÿÿÿÿÿÿÿý

_user_specified_nameInput
Ü
¡
1__inference_Encoding_layer_2_layer_call_fn_459582

inputs
unknown:

	unknown_0:	
identity¢StatefulPartitionedCallå
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:ÿÿÿÿÿÿÿÿÿ*$
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8 *U
fPRN
L__inference_Encoding_layer_2_layer_call_and_return_conditional_losses_458777p
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*(
_output_shapes
:ÿÿÿÿÿÿÿÿÿ`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:ÿÿÿÿÿÿÿÿÿ: : 22
StatefulPartitionedCallStatefulPartitionedCall:P L
(
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
 
_user_specified_nameinputs
ú1

G__inference_VAE-encoder_layer_call_and_return_conditional_losses_458895

inputs+
encoding_layer_1_458761:
ý&
encoding_layer_1_458763:	+
encoding_layer_2_458778:
&
encoding_layer_2_458780:	*
encoding_layer_3_458795:	@%
encoding_layer_3_458797:@)
encoding_layer_4_458812:@ %
encoding_layer_4_458814: 
z_mean_458828: 
z_mean_458830:$
z_log_sigma_458844:  
z_log_sigma_458846:-
classification_layer_458861: )
classification_layer_458863:
identity

identity_1

identity_2

identity_3¢(Encoding_layer_1/StatefulPartitionedCall¢(Encoding_layer_2/StatefulPartitionedCall¢(Encoding_layer_3/StatefulPartitionedCall¢(Encoding_layer_4/StatefulPartitionedCall¢ Sampling/StatefulPartitionedCall¢,classification_layer/StatefulPartitionedCall¢#z_log_sigma/StatefulPartitionedCall¢z_mean/StatefulPartitionedCall
(Encoding_layer_1/StatefulPartitionedCallStatefulPartitionedCallinputsencoding_layer_1_458761encoding_layer_1_458763*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:ÿÿÿÿÿÿÿÿÿ*$
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8 *U
fPRN
L__inference_Encoding_layer_1_layer_call_and_return_conditional_losses_458760¿
(Encoding_layer_2/StatefulPartitionedCallStatefulPartitionedCall1Encoding_layer_1/StatefulPartitionedCall:output:0encoding_layer_2_458778encoding_layer_2_458780*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:ÿÿÿÿÿÿÿÿÿ*$
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8 *U
fPRN
L__inference_Encoding_layer_2_layer_call_and_return_conditional_losses_458777¾
(Encoding_layer_3/StatefulPartitionedCallStatefulPartitionedCall1Encoding_layer_2/StatefulPartitionedCall:output:0encoding_layer_3_458795encoding_layer_3_458797*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ@*$
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8 *U
fPRN
L__inference_Encoding_layer_3_layer_call_and_return_conditional_losses_458794¾
(Encoding_layer_4/StatefulPartitionedCallStatefulPartitionedCall1Encoding_layer_3/StatefulPartitionedCall:output:0encoding_layer_4_458812encoding_layer_4_458814*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ *$
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8 *U
fPRN
L__inference_Encoding_layer_4_layer_call_and_return_conditional_losses_458811
z_mean/StatefulPartitionedCallStatefulPartitionedCall1Encoding_layer_4/StatefulPartitionedCall:output:0z_mean_458828z_mean_458830*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ*$
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8 *K
fFRD
B__inference_z_mean_layer_call_and_return_conditional_losses_458827ª
#z_log_sigma/StatefulPartitionedCallStatefulPartitionedCall1Encoding_layer_4/StatefulPartitionedCall:output:0z_log_sigma_458844z_log_sigma_458846*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ*$
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8 *P
fKRI
G__inference_z_log_sigma_layer_call_and_return_conditional_losses_458843Î
,classification_layer/StatefulPartitionedCallStatefulPartitionedCall1Encoding_layer_4/StatefulPartitionedCall:output:0classification_layer_458861classification_layer_458863*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ*$
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8 *Y
fTRR
P__inference_classification_layer_layer_call_and_return_conditional_losses_458860
 Sampling/StatefulPartitionedCallStatefulPartitionedCall'z_mean/StatefulPartitionedCall:output:0,z_log_sigma/StatefulPartitionedCall:output:0*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ* 
_read_only_resource_inputs
 *0
config_proto 

CPU

GPU2*0J 8 *M
fHRF
D__inference_Sampling_layer_call_and_return_conditional_losses_458889v
IdentityIdentity'z_mean/StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ}

Identity_1Identity,z_log_sigma/StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿz

Identity_2Identity)Sampling/StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ

Identity_3Identity5classification_layer/StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
NoOpNoOp)^Encoding_layer_1/StatefulPartitionedCall)^Encoding_layer_2/StatefulPartitionedCall)^Encoding_layer_3/StatefulPartitionedCall)^Encoding_layer_4/StatefulPartitionedCall!^Sampling/StatefulPartitionedCall-^classification_layer/StatefulPartitionedCall$^z_log_sigma/StatefulPartitionedCall^z_mean/StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0*(
_construction_contextkEagerRuntime*C
_input_shapes2
0:ÿÿÿÿÿÿÿÿÿý: : : : : : : : : : : : : : 2T
(Encoding_layer_1/StatefulPartitionedCall(Encoding_layer_1/StatefulPartitionedCall2T
(Encoding_layer_2/StatefulPartitionedCall(Encoding_layer_2/StatefulPartitionedCall2T
(Encoding_layer_3/StatefulPartitionedCall(Encoding_layer_3/StatefulPartitionedCall2T
(Encoding_layer_4/StatefulPartitionedCall(Encoding_layer_4/StatefulPartitionedCall2D
 Sampling/StatefulPartitionedCall Sampling/StatefulPartitionedCall2\
,classification_layer/StatefulPartitionedCall,classification_layer/StatefulPartitionedCall2J
#z_log_sigma/StatefulPartitionedCall#z_log_sigma/StatefulPartitionedCall2@
z_mean/StatefulPartitionedCallz_mean/StatefulPartitionedCall:P L
(
_output_shapes
:ÿÿÿÿÿÿÿÿÿý
 
_user_specified_nameinputs


,__inference_VAE-encoder_layer_call_fn_459329

inputs
unknown:
ý
	unknown_0:	
	unknown_1:

	unknown_2:	
	unknown_3:	@
	unknown_4:@
	unknown_5:@ 
	unknown_6: 
	unknown_7: 
	unknown_8:
	unknown_9: 

unknown_10:

unknown_11: 

unknown_12:
identity

identity_1

identity_2

identity_3¢StatefulPartitionedCallº
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6	unknown_7	unknown_8	unknown_9
unknown_10
unknown_11
unknown_12*
Tin
2*
Tout
2*
_collective_manager_ids
 *`
_output_shapesN
L:ÿÿÿÿÿÿÿÿÿ:ÿÿÿÿÿÿÿÿÿ:ÿÿÿÿÿÿÿÿÿ:ÿÿÿÿÿÿÿÿÿ*0
_read_only_resource_inputs
	
*0
config_proto 

CPU

GPU2*0J 8 *P
fKRI
G__inference_VAE-encoder_layer_call_and_return_conditional_losses_458895o
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿq

Identity_1Identity StatefulPartitionedCall:output:1^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿq

Identity_2Identity StatefulPartitionedCall:output:2^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿq

Identity_3Identity StatefulPartitionedCall:output:3^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0*(
_construction_contextkEagerRuntime*C
_input_shapes2
0:ÿÿÿÿÿÿÿÿÿý: : : : : : : : : : : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:P L
(
_output_shapes
:ÿÿÿÿÿÿÿÿÿý
 
_user_specified_nameinputs
Ï

$__inference_signature_wrapper_459553	
input
unknown:
ý
	unknown_0:	
	unknown_1:

	unknown_2:	
	unknown_3:	@
	unknown_4:@
	unknown_5:@ 
	unknown_6: 
	unknown_7: 
	unknown_8:
	unknown_9: 

unknown_10:

unknown_11: 

unknown_12:
identity

identity_1

identity_2

identity_3¢StatefulPartitionedCall
StatefulPartitionedCallStatefulPartitionedCallinputunknown	unknown_0	unknown_1	unknown_2	unknown_3	unknown_4	unknown_5	unknown_6	unknown_7	unknown_8	unknown_9
unknown_10
unknown_11
unknown_12*
Tin
2*
Tout
2*
_collective_manager_ids
 *`
_output_shapesN
L:ÿÿÿÿÿÿÿÿÿ:ÿÿÿÿÿÿÿÿÿ:ÿÿÿÿÿÿÿÿÿ:ÿÿÿÿÿÿÿÿÿ*0
_read_only_resource_inputs
	
*0
config_proto 

CPU

GPU2*0J 8 **
f%R#
!__inference__wrapped_model_458742o
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿq

Identity_1Identity StatefulPartitionedCall:output:1^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿq

Identity_2Identity StatefulPartitionedCall:output:2^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿq

Identity_3Identity StatefulPartitionedCall:output:3^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0*(
_construction_contextkEagerRuntime*C
_input_shapes2
0:ÿÿÿÿÿÿÿÿÿý: : : : : : : : : : : : : : 22
StatefulPartitionedCallStatefulPartitionedCall:O K
(
_output_shapes
:ÿÿÿÿÿÿÿÿÿý

_user_specified_nameInput


ý
L__inference_Encoding_layer_4_layer_call_and_return_conditional_losses_458811

inputs0
matmul_readvariableop_resource:@ -
biasadd_readvariableop_resource: 
identity¢BiasAdd/ReadVariableOp¢MatMul/ReadVariableOpt
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

:@ *
dtype0i
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ r
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
: *
dtype0v
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ P
TanhTanhBiasAdd:output:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ W
IdentityIdentityTanh:y:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ w
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:ÿÿÿÿÿÿÿÿÿ@: : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:O K
'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ@
 
_user_specified_nameinputs
Ü
¡
1__inference_Encoding_layer_1_layer_call_fn_459562

inputs
unknown:
ý
	unknown_0:	
identity¢StatefulPartitionedCallå
StatefulPartitionedCallStatefulPartitionedCallinputsunknown	unknown_0*
Tin
2*
Tout
2*
_collective_manager_ids
 *(
_output_shapes
:ÿÿÿÿÿÿÿÿÿ*$
_read_only_resource_inputs
*0
config_proto 

CPU

GPU2*0J 8 *U
fPRN
L__inference_Encoding_layer_1_layer_call_and_return_conditional_losses_458760p
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*(
_output_shapes
:ÿÿÿÿÿÿÿÿÿ`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:ÿÿÿÿÿÿÿÿÿý: : 22
StatefulPartitionedCallStatefulPartitionedCall:P L
(
_output_shapes
:ÿÿÿÿÿÿÿÿÿý
 
_user_specified_nameinputs
¥


L__inference_Encoding_layer_1_layer_call_and_return_conditional_losses_459573

inputs2
matmul_readvariableop_resource:
ý.
biasadd_readvariableop_resource:	
identity¢BiasAdd/ReadVariableOp¢MatMul/ReadVariableOpv
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource* 
_output_shapes
:
ý*
dtype0j
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:ÿÿÿÿÿÿÿÿÿs
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes	
:*
dtype0w
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*(
_output_shapes
:ÿÿÿÿÿÿÿÿÿQ
TanhTanhBiasAdd:output:0*
T0*(
_output_shapes
:ÿÿÿÿÿÿÿÿÿX
IdentityIdentityTanh:y:0^NoOp*
T0*(
_output_shapes
:ÿÿÿÿÿÿÿÿÿw
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:ÿÿÿÿÿÿÿÿÿý: : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:P L
(
_output_shapes
:ÿÿÿÿÿÿÿÿÿý
 
_user_specified_nameinputs
ûY
Ë
G__inference_VAE-encoder_layer_call_and_return_conditional_losses_459512

inputsC
/encoding_layer_1_matmul_readvariableop_resource:
ý?
0encoding_layer_1_biasadd_readvariableop_resource:	C
/encoding_layer_2_matmul_readvariableop_resource:
?
0encoding_layer_2_biasadd_readvariableop_resource:	B
/encoding_layer_3_matmul_readvariableop_resource:	@>
0encoding_layer_3_biasadd_readvariableop_resource:@A
/encoding_layer_4_matmul_readvariableop_resource:@ >
0encoding_layer_4_biasadd_readvariableop_resource: 7
%z_mean_matmul_readvariableop_resource: 4
&z_mean_biasadd_readvariableop_resource:<
*z_log_sigma_matmul_readvariableop_resource: 9
+z_log_sigma_biasadd_readvariableop_resource:E
3classification_layer_matmul_readvariableop_resource: B
4classification_layer_biasadd_readvariableop_resource:
identity

identity_1

identity_2

identity_3¢'Encoding_layer_1/BiasAdd/ReadVariableOp¢&Encoding_layer_1/MatMul/ReadVariableOp¢'Encoding_layer_2/BiasAdd/ReadVariableOp¢&Encoding_layer_2/MatMul/ReadVariableOp¢'Encoding_layer_3/BiasAdd/ReadVariableOp¢&Encoding_layer_3/MatMul/ReadVariableOp¢'Encoding_layer_4/BiasAdd/ReadVariableOp¢&Encoding_layer_4/MatMul/ReadVariableOp¢+classification_layer/BiasAdd/ReadVariableOp¢*classification_layer/MatMul/ReadVariableOp¢"z_log_sigma/BiasAdd/ReadVariableOp¢!z_log_sigma/MatMul/ReadVariableOp¢z_mean/BiasAdd/ReadVariableOp¢z_mean/MatMul/ReadVariableOp
&Encoding_layer_1/MatMul/ReadVariableOpReadVariableOp/encoding_layer_1_matmul_readvariableop_resource* 
_output_shapes
:
ý*
dtype0
Encoding_layer_1/MatMulMatMulinputs.Encoding_layer_1/MatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
'Encoding_layer_1/BiasAdd/ReadVariableOpReadVariableOp0encoding_layer_1_biasadd_readvariableop_resource*
_output_shapes	
:*
dtype0ª
Encoding_layer_1/BiasAddBiasAdd!Encoding_layer_1/MatMul:product:0/Encoding_layer_1/BiasAdd/ReadVariableOp:value:0*
T0*(
_output_shapes
:ÿÿÿÿÿÿÿÿÿs
Encoding_layer_1/TanhTanh!Encoding_layer_1/BiasAdd:output:0*
T0*(
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
&Encoding_layer_2/MatMul/ReadVariableOpReadVariableOp/encoding_layer_2_matmul_readvariableop_resource* 
_output_shapes
:
*
dtype0
Encoding_layer_2/MatMulMatMulEncoding_layer_1/Tanh:y:0.Encoding_layer_2/MatMul/ReadVariableOp:value:0*
T0*(
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
'Encoding_layer_2/BiasAdd/ReadVariableOpReadVariableOp0encoding_layer_2_biasadd_readvariableop_resource*
_output_shapes	
:*
dtype0ª
Encoding_layer_2/BiasAddBiasAdd!Encoding_layer_2/MatMul:product:0/Encoding_layer_2/BiasAdd/ReadVariableOp:value:0*
T0*(
_output_shapes
:ÿÿÿÿÿÿÿÿÿs
Encoding_layer_2/TanhTanh!Encoding_layer_2/BiasAdd:output:0*
T0*(
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
&Encoding_layer_3/MatMul/ReadVariableOpReadVariableOp/encoding_layer_3_matmul_readvariableop_resource*
_output_shapes
:	@*
dtype0
Encoding_layer_3/MatMulMatMulEncoding_layer_2/Tanh:y:0.Encoding_layer_3/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ@
'Encoding_layer_3/BiasAdd/ReadVariableOpReadVariableOp0encoding_layer_3_biasadd_readvariableop_resource*
_output_shapes
:@*
dtype0©
Encoding_layer_3/BiasAddBiasAdd!Encoding_layer_3/MatMul:product:0/Encoding_layer_3/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ@r
Encoding_layer_3/TanhTanh!Encoding_layer_3/BiasAdd:output:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ@
&Encoding_layer_4/MatMul/ReadVariableOpReadVariableOp/encoding_layer_4_matmul_readvariableop_resource*
_output_shapes

:@ *
dtype0
Encoding_layer_4/MatMulMatMulEncoding_layer_3/Tanh:y:0.Encoding_layer_4/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ 
'Encoding_layer_4/BiasAdd/ReadVariableOpReadVariableOp0encoding_layer_4_biasadd_readvariableop_resource*
_output_shapes
: *
dtype0©
Encoding_layer_4/BiasAddBiasAdd!Encoding_layer_4/MatMul:product:0/Encoding_layer_4/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ r
Encoding_layer_4/TanhTanh!Encoding_layer_4/BiasAdd:output:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ 
z_mean/MatMul/ReadVariableOpReadVariableOp%z_mean_matmul_readvariableop_resource*
_output_shapes

: *
dtype0
z_mean/MatMulMatMulEncoding_layer_4/Tanh:y:0$z_mean/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
z_mean/BiasAdd/ReadVariableOpReadVariableOp&z_mean_biasadd_readvariableop_resource*
_output_shapes
:*
dtype0
z_mean/BiasAddBiasAddz_mean/MatMul:product:0%z_mean/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
!z_log_sigma/MatMul/ReadVariableOpReadVariableOp*z_log_sigma_matmul_readvariableop_resource*
_output_shapes

: *
dtype0
z_log_sigma/MatMulMatMulEncoding_layer_4/Tanh:y:0)z_log_sigma/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
"z_log_sigma/BiasAdd/ReadVariableOpReadVariableOp+z_log_sigma_biasadd_readvariableop_resource*
_output_shapes
:*
dtype0
z_log_sigma/BiasAddBiasAddz_log_sigma/MatMul:product:0*z_log_sigma/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
*classification_layer/MatMul/ReadVariableOpReadVariableOp3classification_layer_matmul_readvariableop_resource*
_output_shapes

: *
dtype0¦
classification_layer/MatMulMatMulEncoding_layer_4/Tanh:y:02classification_layer/MatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
+classification_layer/BiasAdd/ReadVariableOpReadVariableOp4classification_layer_biasadd_readvariableop_resource*
_output_shapes
:*
dtype0µ
classification_layer/BiasAddBiasAdd%classification_layer/MatMul:product:03classification_layer/BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
classification_layer/SoftmaxSoftmax%classification_layer/BiasAdd:output:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿU
Sampling/ShapeShapez_mean/BiasAdd:output:0*
T0*
_output_shapes
:f
Sampling/strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: h
Sampling/strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:h
Sampling/strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:þ
Sampling/strided_sliceStridedSliceSampling/Shape:output:0%Sampling/strided_slice/stack:output:0'Sampling/strided_slice/stack_1:output:0'Sampling/strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_maskh
Sampling/strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB:j
 Sampling/strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:j
 Sampling/strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:
Sampling/strided_slice_1StridedSliceSampling/Shape:output:0'Sampling/strided_slice_1/stack:output:0)Sampling/strided_slice_1/stack_1:output:0)Sampling/strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask
Sampling/random_normal/shapePackSampling/strided_slice:output:0!Sampling/strided_slice_1:output:0*
N*
T0*
_output_shapes
:`
Sampling/random_normal/meanConst*
_output_shapes
: *
dtype0*
valueB
 *    b
Sampling/random_normal/stddevConst*
_output_shapes
: *
dtype0*
valueB
 *
×£;©
+Sampling/random_normal/RandomStandardNormalRandomStandardNormal%Sampling/random_normal/shape:output:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ*
dtype0±
Sampling/random_normal/mulMul4Sampling/random_normal/RandomStandardNormal:output:0&Sampling/random_normal/stddev:output:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
Sampling/random_normalAddV2Sampling/random_normal/mul:z:0$Sampling/random_normal/mean:output:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿc
Sampling/ExpExpz_log_sigma/BiasAdd:output:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿs
Sampling/mulMulSampling/Exp:y:0Sampling/random_normal:z:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿr
Sampling/addAddV2z_mean/BiasAdd:output:0Sampling/mul:z:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿf
IdentityIdentityz_mean/BiasAdd:output:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿm

Identity_1Identityz_log_sigma/BiasAdd:output:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿa

Identity_2IdentitySampling/add:z:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿw

Identity_3Identity&classification_layer/Softmax:softmax:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿõ
NoOpNoOp(^Encoding_layer_1/BiasAdd/ReadVariableOp'^Encoding_layer_1/MatMul/ReadVariableOp(^Encoding_layer_2/BiasAdd/ReadVariableOp'^Encoding_layer_2/MatMul/ReadVariableOp(^Encoding_layer_3/BiasAdd/ReadVariableOp'^Encoding_layer_3/MatMul/ReadVariableOp(^Encoding_layer_4/BiasAdd/ReadVariableOp'^Encoding_layer_4/MatMul/ReadVariableOp,^classification_layer/BiasAdd/ReadVariableOp+^classification_layer/MatMul/ReadVariableOp#^z_log_sigma/BiasAdd/ReadVariableOp"^z_log_sigma/MatMul/ReadVariableOp^z_mean/BiasAdd/ReadVariableOp^z_mean/MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0"!

identity_1Identity_1:output:0"!

identity_2Identity_2:output:0"!

identity_3Identity_3:output:0*(
_construction_contextkEagerRuntime*C
_input_shapes2
0:ÿÿÿÿÿÿÿÿÿý: : : : : : : : : : : : : : 2R
'Encoding_layer_1/BiasAdd/ReadVariableOp'Encoding_layer_1/BiasAdd/ReadVariableOp2P
&Encoding_layer_1/MatMul/ReadVariableOp&Encoding_layer_1/MatMul/ReadVariableOp2R
'Encoding_layer_2/BiasAdd/ReadVariableOp'Encoding_layer_2/BiasAdd/ReadVariableOp2P
&Encoding_layer_2/MatMul/ReadVariableOp&Encoding_layer_2/MatMul/ReadVariableOp2R
'Encoding_layer_3/BiasAdd/ReadVariableOp'Encoding_layer_3/BiasAdd/ReadVariableOp2P
&Encoding_layer_3/MatMul/ReadVariableOp&Encoding_layer_3/MatMul/ReadVariableOp2R
'Encoding_layer_4/BiasAdd/ReadVariableOp'Encoding_layer_4/BiasAdd/ReadVariableOp2P
&Encoding_layer_4/MatMul/ReadVariableOp&Encoding_layer_4/MatMul/ReadVariableOp2Z
+classification_layer/BiasAdd/ReadVariableOp+classification_layer/BiasAdd/ReadVariableOp2X
*classification_layer/MatMul/ReadVariableOp*classification_layer/MatMul/ReadVariableOp2H
"z_log_sigma/BiasAdd/ReadVariableOp"z_log_sigma/BiasAdd/ReadVariableOp2F
!z_log_sigma/MatMul/ReadVariableOp!z_log_sigma/MatMul/ReadVariableOp2>
z_mean/BiasAdd/ReadVariableOpz_mean/BiasAdd/ReadVariableOp2<
z_mean/MatMul/ReadVariableOpz_mean/MatMul/ReadVariableOp:P L
(
_output_shapes
:ÿÿÿÿÿÿÿÿÿý
 
_user_specified_nameinputs
÷
r
)__inference_Sampling_layer_call_fn_459683
inputs_0
inputs_1
identity¢StatefulPartitionedCallÏ
StatefulPartitionedCallStatefulPartitionedCallinputs_0inputs_1*
Tin
2*
Tout
2*
_collective_manager_ids
 *'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ* 
_read_only_resource_inputs
 *0
config_proto 

CPU

GPU2*0J 8 *M
fHRF
D__inference_Sampling_layer_call_and_return_conditional_losses_458965o
IdentityIdentity StatefulPartitionedCall:output:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ`
NoOpNoOp^StatefulPartitionedCall*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:ÿÿÿÿÿÿÿÿÿ:ÿÿÿÿÿÿÿÿÿ22
StatefulPartitionedCallStatefulPartitionedCall:Q M
'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
"
_user_specified_name
inputs/0:QM
'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
"
_user_specified_name
inputs/1


ý
L__inference_Encoding_layer_4_layer_call_and_return_conditional_losses_459633

inputs0
matmul_readvariableop_resource:@ -
biasadd_readvariableop_resource: 
identity¢BiasAdd/ReadVariableOp¢MatMul/ReadVariableOpt
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes

:@ *
dtype0i
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ r
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
: *
dtype0v
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ P
TanhTanhBiasAdd:output:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ W
IdentityIdentityTanh:y:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ w
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime**
_input_shapes
:ÿÿÿÿÿÿÿÿÿ@: : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:O K
'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ@
 
_user_specified_nameinputs


þ
L__inference_Encoding_layer_3_layer_call_and_return_conditional_losses_458794

inputs1
matmul_readvariableop_resource:	@-
biasadd_readvariableop_resource:@
identity¢BiasAdd/ReadVariableOp¢MatMul/ReadVariableOpu
MatMul/ReadVariableOpReadVariableOpmatmul_readvariableop_resource*
_output_shapes
:	@*
dtype0i
MatMulMatMulinputsMatMul/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ@r
BiasAdd/ReadVariableOpReadVariableOpbiasadd_readvariableop_resource*
_output_shapes
:@*
dtype0v
BiasAddBiasAddMatMul:product:0BiasAdd/ReadVariableOp:value:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ@P
TanhTanhBiasAdd:output:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ@W
IdentityIdentityTanh:y:0^NoOp*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ@w
NoOpNoOp^BiasAdd/ReadVariableOp^MatMul/ReadVariableOp*"
_acd_function_control_output(*
_output_shapes
 "
identityIdentity:output:0*(
_construction_contextkEagerRuntime*+
_input_shapes
:ÿÿÿÿÿÿÿÿÿ: : 20
BiasAdd/ReadVariableOpBiasAdd/ReadVariableOp2.
MatMul/ReadVariableOpMatMul/ReadVariableOp:P L
(
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
 
_user_specified_nameinputs
þ
s
D__inference_Sampling_layer_call_and_return_conditional_losses_459729
inputs_0
inputs_1
identity=
ShapeShapeinputs_0*
T0*
_output_shapes
:]
strided_slice/stackConst*
_output_shapes
:*
dtype0*
valueB: _
strided_slice/stack_1Const*
_output_shapes
:*
dtype0*
valueB:_
strided_slice/stack_2Const*
_output_shapes
:*
dtype0*
valueB:Ñ
strided_sliceStridedSliceShape:output:0strided_slice/stack:output:0strided_slice/stack_1:output:0strided_slice/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask_
strided_slice_1/stackConst*
_output_shapes
:*
dtype0*
valueB:a
strided_slice_1/stack_1Const*
_output_shapes
:*
dtype0*
valueB:a
strided_slice_1/stack_2Const*
_output_shapes
:*
dtype0*
valueB:Ù
strided_slice_1StridedSliceShape:output:0strided_slice_1/stack:output:0 strided_slice_1/stack_1:output:0 strided_slice_1/stack_2:output:0*
Index0*
T0*
_output_shapes
: *
shrink_axis_mask{
random_normal/shapePackstrided_slice:output:0strided_slice_1:output:0*
N*
T0*
_output_shapes
:W
random_normal/meanConst*
_output_shapes
: *
dtype0*
valueB
 *    Y
random_normal/stddevConst*
_output_shapes
: *
dtype0*
valueB
 *
×£;
"random_normal/RandomStandardNormalRandomStandardNormalrandom_normal/shape:output:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ*
dtype0
random_normal/mulMul+random_normal/RandomStandardNormal:output:0random_normal/stddev:output:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ|
random_normalAddV2random_normal/mul:z:0random_normal/mean:output:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿF
ExpExpinputs_1*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿX
mulMulExp:y:0random_normal:z:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿQ
addAddV2inputs_0mul:z:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿO
IdentityIdentityadd:z:0*
T0*'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ"
identityIdentity:output:0*(
_construction_contextkEagerRuntime*9
_input_shapes(
&:ÿÿÿÿÿÿÿÿÿ:ÿÿÿÿÿÿÿÿÿ:Q M
'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
"
_user_specified_name
inputs/0:QM
'
_output_shapes
:ÿÿÿÿÿÿÿÿÿ
"
_user_specified_name
inputs/1"ÛL
saver_filename:0StatefulPartitionedCall_1:0StatefulPartitionedCall_28"
saved_model_main_op

NoOp*>
__saved_model_init_op%#
__saved_model_init_op

NoOp*ï
serving_defaultÛ
8
Input/
serving_default_Input:0ÿÿÿÿÿÿÿÿÿý<
Sampling0
StatefulPartitionedCall:0ÿÿÿÿÿÿÿÿÿH
classification_layer0
StatefulPartitionedCall:1ÿÿÿÿÿÿÿÿÿ?
z_log_sigma0
StatefulPartitionedCall:2ÿÿÿÿÿÿÿÿÿ:
z_mean0
StatefulPartitionedCall:3ÿÿÿÿÿÿÿÿÿtensorflow/serving/predict:¦

layer-0
layer_with_weights-0
layer-1
layer_with_weights-1
layer-2
layer_with_weights-2
layer-3
layer_with_weights-3
layer-4
layer_with_weights-4
layer-5
layer_with_weights-5
layer-6
layer-7
	layer_with_weights-6
	layer-8

	optimizer
loss
	variables
trainable_variables
regularization_losses
	keras_api
__call__
*&call_and_return_all_conditional_losses
_default_save_signature

signatures"
_tf_keras_network
"
_tf_keras_input_layer
»

kernel
bias
	variables
trainable_variables
regularization_losses
	keras_api
__call__
*&call_and_return_all_conditional_losses"
_tf_keras_layer
»

kernel
bias
	variables
trainable_variables
 regularization_losses
!	keras_api
"__call__
*#&call_and_return_all_conditional_losses"
_tf_keras_layer
»

$kernel
%bias
&	variables
'trainable_variables
(regularization_losses
)	keras_api
*__call__
*+&call_and_return_all_conditional_losses"
_tf_keras_layer
»

,kernel
-bias
.	variables
/trainable_variables
0regularization_losses
1	keras_api
2__call__
*3&call_and_return_all_conditional_losses"
_tf_keras_layer
»

4kernel
5bias
6	variables
7trainable_variables
8regularization_losses
9	keras_api
:__call__
*;&call_and_return_all_conditional_losses"
_tf_keras_layer
»

<kernel
=bias
>	variables
?trainable_variables
@regularization_losses
A	keras_api
B__call__
*C&call_and_return_all_conditional_losses"
_tf_keras_layer
¥
D	variables
Etrainable_variables
Fregularization_losses
G	keras_api
H__call__
*I&call_and_return_all_conditional_losses"
_tf_keras_layer
»

Jkernel
Kbias
L	variables
Mtrainable_variables
Nregularization_losses
O	keras_api
P__call__
*Q&call_and_return_all_conditional_losses"
_tf_keras_layer
ë

Rbeta_1

Sbeta_2
	Tdecay
Ulearning_rate
Vitermmmm$m%m,m-m4m5m<m=mJmKmvvvv$v%v,v-v4v 5v¡<v¢=v£Jv¤Kv¥"
	optimizer
 "
trackable_dict_wrapper

0
1
2
3
$4
%5
,6
-7
48
59
<10
=11
J12
K13"
trackable_list_wrapper

0
1
2
3
$4
%5
,6
-7
48
59
<10
=11
J12
K13"
trackable_list_wrapper
 "
trackable_list_wrapper
Ê
Wnon_trainable_variables

Xlayers
Ymetrics
Zlayer_regularization_losses
[layer_metrics
	variables
trainable_variables
regularization_losses
__call__
_default_save_signature
*&call_and_return_all_conditional_losses
&"call_and_return_conditional_losses"
_generic_user_object
þ2û
,__inference_VAE-encoder_layer_call_fn_458932
,__inference_VAE-encoder_layer_call_fn_459329
,__inference_VAE-encoder_layer_call_fn_459368
,__inference_VAE-encoder_layer_call_fn_459198À
·²³
FullArgSpec1
args)&
jself
jinputs

jtraining
jmask
varargs
 
varkw
 
defaults
p 

 

kwonlyargs 
kwonlydefaultsª 
annotationsª *
 
ê2ç
G__inference_VAE-encoder_layer_call_and_return_conditional_losses_459440
G__inference_VAE-encoder_layer_call_and_return_conditional_losses_459512
G__inference_VAE-encoder_layer_call_and_return_conditional_losses_459241
G__inference_VAE-encoder_layer_call_and_return_conditional_losses_459284À
·²³
FullArgSpec1
args)&
jself
jinputs

jtraining
jmask
varargs
 
varkw
 
defaults
p 

 

kwonlyargs 
kwonlydefaultsª 
annotationsª *
 
ÊBÇ
!__inference__wrapped_model_458742Input"
²
FullArgSpec
args 
varargsjargs
varkwjkwargs
defaults
 

kwonlyargs 
kwonlydefaults
 
annotationsª *
 
,
\serving_default"
signature_map
+:)
ý2Encoding_layer_1/kernel
$:"2Encoding_layer_1/bias
.
0
1"
trackable_list_wrapper
.
0
1"
trackable_list_wrapper
 "
trackable_list_wrapper
­
]non_trainable_variables

^layers
_metrics
`layer_regularization_losses
alayer_metrics
	variables
trainable_variables
regularization_losses
__call__
*&call_and_return_all_conditional_losses
&"call_and_return_conditional_losses"
_generic_user_object
Û2Ø
1__inference_Encoding_layer_1_layer_call_fn_459562¢
²
FullArgSpec
args
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs 
kwonlydefaults
 
annotationsª *
 
ö2ó
L__inference_Encoding_layer_1_layer_call_and_return_conditional_losses_459573¢
²
FullArgSpec
args
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs 
kwonlydefaults
 
annotationsª *
 
+:)
2Encoding_layer_2/kernel
$:"2Encoding_layer_2/bias
.
0
1"
trackable_list_wrapper
.
0
1"
trackable_list_wrapper
 "
trackable_list_wrapper
­
bnon_trainable_variables

clayers
dmetrics
elayer_regularization_losses
flayer_metrics
	variables
trainable_variables
 regularization_losses
"__call__
*#&call_and_return_all_conditional_losses
&#"call_and_return_conditional_losses"
_generic_user_object
Û2Ø
1__inference_Encoding_layer_2_layer_call_fn_459582¢
²
FullArgSpec
args
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs 
kwonlydefaults
 
annotationsª *
 
ö2ó
L__inference_Encoding_layer_2_layer_call_and_return_conditional_losses_459593¢
²
FullArgSpec
args
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs 
kwonlydefaults
 
annotationsª *
 
*:(	@2Encoding_layer_3/kernel
#:!@2Encoding_layer_3/bias
.
$0
%1"
trackable_list_wrapper
.
$0
%1"
trackable_list_wrapper
 "
trackable_list_wrapper
­
gnon_trainable_variables

hlayers
imetrics
jlayer_regularization_losses
klayer_metrics
&	variables
'trainable_variables
(regularization_losses
*__call__
*+&call_and_return_all_conditional_losses
&+"call_and_return_conditional_losses"
_generic_user_object
Û2Ø
1__inference_Encoding_layer_3_layer_call_fn_459602¢
²
FullArgSpec
args
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs 
kwonlydefaults
 
annotationsª *
 
ö2ó
L__inference_Encoding_layer_3_layer_call_and_return_conditional_losses_459613¢
²
FullArgSpec
args
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs 
kwonlydefaults
 
annotationsª *
 
):'@ 2Encoding_layer_4/kernel
#:! 2Encoding_layer_4/bias
.
,0
-1"
trackable_list_wrapper
.
,0
-1"
trackable_list_wrapper
 "
trackable_list_wrapper
­
lnon_trainable_variables

mlayers
nmetrics
olayer_regularization_losses
player_metrics
.	variables
/trainable_variables
0regularization_losses
2__call__
*3&call_and_return_all_conditional_losses
&3"call_and_return_conditional_losses"
_generic_user_object
Û2Ø
1__inference_Encoding_layer_4_layer_call_fn_459622¢
²
FullArgSpec
args
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs 
kwonlydefaults
 
annotationsª *
 
ö2ó
L__inference_Encoding_layer_4_layer_call_and_return_conditional_losses_459633¢
²
FullArgSpec
args
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs 
kwonlydefaults
 
annotationsª *
 
: 2z_mean/kernel
:2z_mean/bias
.
40
51"
trackable_list_wrapper
.
40
51"
trackable_list_wrapper
 "
trackable_list_wrapper
­
qnon_trainable_variables

rlayers
smetrics
tlayer_regularization_losses
ulayer_metrics
6	variables
7trainable_variables
8regularization_losses
:__call__
*;&call_and_return_all_conditional_losses
&;"call_and_return_conditional_losses"
_generic_user_object
Ñ2Î
'__inference_z_mean_layer_call_fn_459642¢
²
FullArgSpec
args
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs 
kwonlydefaults
 
annotationsª *
 
ì2é
B__inference_z_mean_layer_call_and_return_conditional_losses_459652¢
²
FullArgSpec
args
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs 
kwonlydefaults
 
annotationsª *
 
$:" 2z_log_sigma/kernel
:2z_log_sigma/bias
.
<0
=1"
trackable_list_wrapper
.
<0
=1"
trackable_list_wrapper
 "
trackable_list_wrapper
­
vnon_trainable_variables

wlayers
xmetrics
ylayer_regularization_losses
zlayer_metrics
>	variables
?trainable_variables
@regularization_losses
B__call__
*C&call_and_return_all_conditional_losses
&C"call_and_return_conditional_losses"
_generic_user_object
Ö2Ó
,__inference_z_log_sigma_layer_call_fn_459661¢
²
FullArgSpec
args
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs 
kwonlydefaults
 
annotationsª *
 
ñ2î
G__inference_z_log_sigma_layer_call_and_return_conditional_losses_459671¢
²
FullArgSpec
args
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs 
kwonlydefaults
 
annotationsª *
 
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
­
{non_trainable_variables

|layers
}metrics
~layer_regularization_losses
layer_metrics
D	variables
Etrainable_variables
Fregularization_losses
H__call__
*I&call_and_return_all_conditional_losses
&I"call_and_return_conditional_losses"
_generic_user_object
2
)__inference_Sampling_layer_call_fn_459677
)__inference_Sampling_layer_call_fn_459683À
·²³
FullArgSpec1
args)&
jself
jinputs
jmask

jtraining
varargs
 
varkw
 
defaults

 
p 

kwonlyargs 
kwonlydefaultsª 
annotationsª *
 
Ò2Ï
D__inference_Sampling_layer_call_and_return_conditional_losses_459706
D__inference_Sampling_layer_call_and_return_conditional_losses_459729À
·²³
FullArgSpec1
args)&
jself
jinputs
jmask

jtraining
varargs
 
varkw
 
defaults

 
p 

kwonlyargs 
kwonlydefaultsª 
annotationsª *
 
-:+ 2classification_layer/kernel
':%2classification_layer/bias
.
J0
K1"
trackable_list_wrapper
.
J0
K1"
trackable_list_wrapper
 "
trackable_list_wrapper
²
non_trainable_variables
layers
metrics
 layer_regularization_losses
layer_metrics
L	variables
Mtrainable_variables
Nregularization_losses
P__call__
*Q&call_and_return_all_conditional_losses
&Q"call_and_return_conditional_losses"
_generic_user_object
ß2Ü
5__inference_classification_layer_layer_call_fn_459738¢
²
FullArgSpec
args
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs 
kwonlydefaults
 
annotationsª *
 
ú2÷
P__inference_classification_layer_layer_call_and_return_conditional_losses_459749¢
²
FullArgSpec
args
jself
jinputs
varargs
 
varkw
 
defaults
 

kwonlyargs 
kwonlydefaults
 
annotationsª *
 
: (2beta_1
: (2beta_2
: (2decay
: (2learning_rate
:	 (2	Adam/iter
 "
trackable_list_wrapper
_
0
1
2
3
4
5
6
7
	8"
trackable_list_wrapper
(
0"
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
ÉBÆ
$__inference_signature_wrapper_459553Input"
²
FullArgSpec
args 
varargs
 
varkwjkwargs
defaults
 

kwonlyargs 
kwonlydefaults
 
annotationsª *
 
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_list_wrapper
 "
trackable_dict_wrapper
R

total

count
	variables
	keras_api"
_tf_keras_metric
:  (2total
:  (2count
0
0
1"
trackable_list_wrapper
.
	variables"
_generic_user_object
0:.
ý2Adam/Encoding_layer_1/kernel/m
):'2Adam/Encoding_layer_1/bias/m
0:.
2Adam/Encoding_layer_2/kernel/m
):'2Adam/Encoding_layer_2/bias/m
/:-	@2Adam/Encoding_layer_3/kernel/m
(:&@2Adam/Encoding_layer_3/bias/m
.:,@ 2Adam/Encoding_layer_4/kernel/m
(:& 2Adam/Encoding_layer_4/bias/m
$:" 2Adam/z_mean/kernel/m
:2Adam/z_mean/bias/m
):' 2Adam/z_log_sigma/kernel/m
#:!2Adam/z_log_sigma/bias/m
2:0 2"Adam/classification_layer/kernel/m
,:*2 Adam/classification_layer/bias/m
0:.
ý2Adam/Encoding_layer_1/kernel/v
):'2Adam/Encoding_layer_1/bias/v
0:.
2Adam/Encoding_layer_2/kernel/v
):'2Adam/Encoding_layer_2/bias/v
/:-	@2Adam/Encoding_layer_3/kernel/v
(:&@2Adam/Encoding_layer_3/bias/v
.:,@ 2Adam/Encoding_layer_4/kernel/v
(:& 2Adam/Encoding_layer_4/bias/v
$:" 2Adam/z_mean/kernel/v
:2Adam/z_mean/bias/v
):' 2Adam/z_log_sigma/kernel/v
#:!2Adam/z_log_sigma/bias/v
2:0 2"Adam/classification_layer/kernel/v
,:*2 Adam/classification_layer/bias/v®
L__inference_Encoding_layer_1_layer_call_and_return_conditional_losses_459573^0¢-
&¢#
!
inputsÿÿÿÿÿÿÿÿÿý
ª "&¢#

0ÿÿÿÿÿÿÿÿÿ
 
1__inference_Encoding_layer_1_layer_call_fn_459562Q0¢-
&¢#
!
inputsÿÿÿÿÿÿÿÿÿý
ª "ÿÿÿÿÿÿÿÿÿ®
L__inference_Encoding_layer_2_layer_call_and_return_conditional_losses_459593^0¢-
&¢#
!
inputsÿÿÿÿÿÿÿÿÿ
ª "&¢#

0ÿÿÿÿÿÿÿÿÿ
 
1__inference_Encoding_layer_2_layer_call_fn_459582Q0¢-
&¢#
!
inputsÿÿÿÿÿÿÿÿÿ
ª "ÿÿÿÿÿÿÿÿÿ­
L__inference_Encoding_layer_3_layer_call_and_return_conditional_losses_459613]$%0¢-
&¢#
!
inputsÿÿÿÿÿÿÿÿÿ
ª "%¢"

0ÿÿÿÿÿÿÿÿÿ@
 
1__inference_Encoding_layer_3_layer_call_fn_459602P$%0¢-
&¢#
!
inputsÿÿÿÿÿÿÿÿÿ
ª "ÿÿÿÿÿÿÿÿÿ@¬
L__inference_Encoding_layer_4_layer_call_and_return_conditional_losses_459633\,-/¢,
%¢"
 
inputsÿÿÿÿÿÿÿÿÿ@
ª "%¢"

0ÿÿÿÿÿÿÿÿÿ 
 
1__inference_Encoding_layer_4_layer_call_fn_459622O,-/¢,
%¢"
 
inputsÿÿÿÿÿÿÿÿÿ@
ª "ÿÿÿÿÿÿÿÿÿ Ô
D__inference_Sampling_layer_call_and_return_conditional_losses_459706b¢_
X¢U
KH
"
inputs/0ÿÿÿÿÿÿÿÿÿ
"
inputs/1ÿÿÿÿÿÿÿÿÿ

 
p 
ª "%¢"

0ÿÿÿÿÿÿÿÿÿ
 Ô
D__inference_Sampling_layer_call_and_return_conditional_losses_459729b¢_
X¢U
KH
"
inputs/0ÿÿÿÿÿÿÿÿÿ
"
inputs/1ÿÿÿÿÿÿÿÿÿ

 
p
ª "%¢"

0ÿÿÿÿÿÿÿÿÿ
 «
)__inference_Sampling_layer_call_fn_459677~b¢_
X¢U
KH
"
inputs/0ÿÿÿÿÿÿÿÿÿ
"
inputs/1ÿÿÿÿÿÿÿÿÿ

 
p 
ª "ÿÿÿÿÿÿÿÿÿ«
)__inference_Sampling_layer_call_fn_459683~b¢_
X¢U
KH
"
inputs/0ÿÿÿÿÿÿÿÿÿ
"
inputs/1ÿÿÿÿÿÿÿÿÿ

 
p
ª "ÿÿÿÿÿÿÿÿÿ¢
G__inference_VAE-encoder_layer_call_and_return_conditional_losses_459241Ö$%,-45<=JK7¢4
-¢*
 
Inputÿÿÿÿÿÿÿÿÿý
p 

 
ª "¢
|

0/0ÿÿÿÿÿÿÿÿÿ

0/1ÿÿÿÿÿÿÿÿÿ

0/2ÿÿÿÿÿÿÿÿÿ

0/3ÿÿÿÿÿÿÿÿÿ
 ¢
G__inference_VAE-encoder_layer_call_and_return_conditional_losses_459284Ö$%,-45<=JK7¢4
-¢*
 
Inputÿÿÿÿÿÿÿÿÿý
p

 
ª "¢
|

0/0ÿÿÿÿÿÿÿÿÿ

0/1ÿÿÿÿÿÿÿÿÿ

0/2ÿÿÿÿÿÿÿÿÿ

0/3ÿÿÿÿÿÿÿÿÿ
 £
G__inference_VAE-encoder_layer_call_and_return_conditional_losses_459440×$%,-45<=JK8¢5
.¢+
!
inputsÿÿÿÿÿÿÿÿÿý
p 

 
ª "¢
|

0/0ÿÿÿÿÿÿÿÿÿ

0/1ÿÿÿÿÿÿÿÿÿ

0/2ÿÿÿÿÿÿÿÿÿ

0/3ÿÿÿÿÿÿÿÿÿ
 £
G__inference_VAE-encoder_layer_call_and_return_conditional_losses_459512×$%,-45<=JK8¢5
.¢+
!
inputsÿÿÿÿÿÿÿÿÿý
p

 
ª "¢
|

0/0ÿÿÿÿÿÿÿÿÿ

0/1ÿÿÿÿÿÿÿÿÿ

0/2ÿÿÿÿÿÿÿÿÿ

0/3ÿÿÿÿÿÿÿÿÿ
 ó
,__inference_VAE-encoder_layer_call_fn_458932Â$%,-45<=JK7¢4
-¢*
 
Inputÿÿÿÿÿÿÿÿÿý
p 

 
ª "wt

0ÿÿÿÿÿÿÿÿÿ

1ÿÿÿÿÿÿÿÿÿ

2ÿÿÿÿÿÿÿÿÿ

3ÿÿÿÿÿÿÿÿÿó
,__inference_VAE-encoder_layer_call_fn_459198Â$%,-45<=JK7¢4
-¢*
 
Inputÿÿÿÿÿÿÿÿÿý
p

 
ª "wt

0ÿÿÿÿÿÿÿÿÿ

1ÿÿÿÿÿÿÿÿÿ

2ÿÿÿÿÿÿÿÿÿ

3ÿÿÿÿÿÿÿÿÿô
,__inference_VAE-encoder_layer_call_fn_459329Ã$%,-45<=JK8¢5
.¢+
!
inputsÿÿÿÿÿÿÿÿÿý
p 

 
ª "wt

0ÿÿÿÿÿÿÿÿÿ

1ÿÿÿÿÿÿÿÿÿ

2ÿÿÿÿÿÿÿÿÿ

3ÿÿÿÿÿÿÿÿÿô
,__inference_VAE-encoder_layer_call_fn_459368Ã$%,-45<=JK8¢5
.¢+
!
inputsÿÿÿÿÿÿÿÿÿý
p

 
ª "wt

0ÿÿÿÿÿÿÿÿÿ

1ÿÿÿÿÿÿÿÿÿ

2ÿÿÿÿÿÿÿÿÿ

3ÿÿÿÿÿÿÿÿÿÈ
!__inference__wrapped_model_458742¢$%,-45<=JK/¢,
%¢"
 
Inputÿÿÿÿÿÿÿÿÿý
ª "ÞªÚ
.
Sampling"
Samplingÿÿÿÿÿÿÿÿÿ
F
classification_layer.+
classification_layerÿÿÿÿÿÿÿÿÿ
4
z_log_sigma%"
z_log_sigmaÿÿÿÿÿÿÿÿÿ
*
z_mean 
z_meanÿÿÿÿÿÿÿÿÿ°
P__inference_classification_layer_layer_call_and_return_conditional_losses_459749\JK/¢,
%¢"
 
inputsÿÿÿÿÿÿÿÿÿ 
ª "%¢"

0ÿÿÿÿÿÿÿÿÿ
 
5__inference_classification_layer_layer_call_fn_459738OJK/¢,
%¢"
 
inputsÿÿÿÿÿÿÿÿÿ 
ª "ÿÿÿÿÿÿÿÿÿÔ
$__inference_signature_wrapper_459553«$%,-45<=JK8¢5
¢ 
.ª+
)
Input 
Inputÿÿÿÿÿÿÿÿÿý"ÞªÚ
.
Sampling"
Samplingÿÿÿÿÿÿÿÿÿ
F
classification_layer.+
classification_layerÿÿÿÿÿÿÿÿÿ
4
z_log_sigma%"
z_log_sigmaÿÿÿÿÿÿÿÿÿ
*
z_mean 
z_meanÿÿÿÿÿÿÿÿÿ§
G__inference_z_log_sigma_layer_call_and_return_conditional_losses_459671\<=/¢,
%¢"
 
inputsÿÿÿÿÿÿÿÿÿ 
ª "%¢"

0ÿÿÿÿÿÿÿÿÿ
 
,__inference_z_log_sigma_layer_call_fn_459661O<=/¢,
%¢"
 
inputsÿÿÿÿÿÿÿÿÿ 
ª "ÿÿÿÿÿÿÿÿÿ¢
B__inference_z_mean_layer_call_and_return_conditional_losses_459652\45/¢,
%¢"
 
inputsÿÿÿÿÿÿÿÿÿ 
ª "%¢"

0ÿÿÿÿÿÿÿÿÿ
 z
'__inference_z_mean_layer_call_fn_459642O45/¢,
%¢"
 
inputsÿÿÿÿÿÿÿÿÿ 
ª "ÿÿÿÿÿÿÿÿÿ