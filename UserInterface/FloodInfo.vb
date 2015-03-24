Public Class FloodInfo

    Public Const maxFloodEvents As Integer = 30


    Public Shared ReadOnly eventNumber As Label() = {Form1.Event1, Form1.Event2, Form1.Event3, Form1.Event4, Form1.Event5, Form1.Event6, Form1.Event7,
                               Form1.Event8, Form1.Event9, Form1.Event10, Form1.Event11, Form1.Event12, Form1.Event13, Form1.Event14, Form1.Event15,
                               Form1.Event16, Form1.Event17, Form1.Event18, Form1.Event19, Form1.Event20, Form1.Event21, Form1.Event22, Form1.Event23,
                               Form1.Event24, Form1.Event25, Form1.Event26, Form1.Event27, Form1.Event28, Form1.Event29, Form1.Event30}


    Public Shared ReadOnly day As TextBox() = {Form1.TB_FloodDay1, Form1.TB_FloodDay2, Form1.TB_FloodDay3, Form1.TB_FloodDay4, Form1.TB_FloodDay5, Form1.TB_FloodDay6, Form1.TB_FloodDay7,
                               Form1.TB_FloodDay8, Form1.TB_FloodDay9, Form1.TB_FloodDay10, Form1.TB_FloodDay11, Form1.TB_FloodDay12, Form1.TB_FloodDay13, Form1.TB_FloodDay14, Form1.TB_FloodDay15,
                               Form1.TB_FloodDay16, Form1.TB_FloodDay17, Form1.TB_FloodDay18, Form1.TB_FloodDay19, Form1.TB_FloodDay20, Form1.TB_FloodDay21, Form1.TB_FloodDay22, Form1.TB_FloodDay23,
                               Form1.TB_FloodDay24, Form1.TB_FloodDay25, Form1.TB_FloodDay26, Form1.TB_FloodDay27, Form1.TB_FloodDay28, Form1.TB_FloodDay29, Form1.TB_FloodDay30}


    Public Shared ReadOnly fill As TextBox() = {Form1.TB_Fill1, Form1.TB_Fill2, Form1.TB_Fill3, Form1.TB_Fill4, Form1.TB_Fill5, Form1.TB_Fill6, Form1.TB_Fill7,
                               Form1.TB_Fill8, Form1.TB_Fill9, Form1.TB_Fill10, Form1.TB_Fill11, Form1.TB_Fill12, Form1.TB_Fill13, Form1.TB_Fill14, Form1.TB_Fill15,
                               Form1.TB_Fill16, Form1.TB_Fill17, Form1.TB_Fill18, Form1.TB_Fill19, Form1.TB_Fill20, Form1.TB_Fill21, Form1.TB_Fill22, Form1.TB_Fill23,
                               Form1.TB_Fill24, Form1.TB_Fill25, Form1.TB_Fill26, Form1.TB_Fill27, Form1.TB_Fill28, Form1.TB_Fill29, Form1.TB_Fill30}


    Public Shared ReadOnly wier As TextBox() = {Form1.TB_FloodWier1, Form1.TB_FloodWier2, Form1.TB_FloodWier3, Form1.TB_FloodWier4, Form1.TB_FloodWier5, Form1.TB_FloodWier6, Form1.TB_FloodWier7,
                               Form1.TB_FloodWier8, Form1.TB_FloodWier9, Form1.TB_FloodWier10, Form1.TB_FloodWier11, Form1.TB_FloodWier12, Form1.TB_FloodWier13, Form1.TB_FloodWier14, Form1.TB_FloodWier15,
                               Form1.TB_FloodWier16, Form1.TB_FloodWier17, Form1.TB_FloodWier18, Form1.TB_FloodWier19, Form1.TB_FloodWier20, Form1.TB_FloodWier21, Form1.TB_FloodWier22, Form1.TB_FloodWier23,
                               Form1.TB_FloodWier24, Form1.TB_FloodWier25, Form1.TB_FloodWier26, Form1.TB_FloodWier27, Form1.TB_FloodWier28, Form1.TB_FloodWier29, Form1.TB_FloodWier30}


    Public Shared ReadOnly min As TextBox() = {Form1.TB_Minimum1, Form1.TB_Minimum2, Form1.TB_Minimum3, Form1.TB_Minimum4, Form1.TB_Minimum5, Form1.TB_Minimum6, Form1.TB_Minimum7,
                               Form1.TB_Minimum8, Form1.TB_Minimum9, Form1.TB_Minimum10, Form1.TB_Minimum11, Form1.TB_Minimum12, Form1.TB_Minimum13, Form1.TB_Minimum14, Form1.TB_Minimum15,
                               Form1.TB_Minimum16, Form1.TB_Minimum17, Form1.TB_Minimum18, Form1.TB_Minimum19, Form1.TB_Minimum20, Form1.TB_Minimum21, Form1.TB_Minimum22, Form1.TB_Minimum23,
                               Form1.TB_Minimum24, Form1.TB_Minimum25, Form1.TB_Minimum26, Form1.TB_Minimum27, Form1.TB_Minimum28, Form1.TB_Minimum29, Form1.TB_Minimum30}

    Public Shared ReadOnly turnOver As TextBox() = {Form1.TB_FloodTO1, Form1.TB_FloodTO2, Form1.TB_FloodTO3, Form1.TB_FloodTO4, Form1.TB_FloodTO5, Form1.TB_FloodTO6, Form1.TB_FloodTO7,
                              Form1.TB_FloodTO8, Form1.TB_FloodTO9, Form1.TB_FloodTO10, Form1.TB_FloodTO11, Form1.TB_FloodTO12, Form1.TB_FloodTO13, Form1.TB_FloodTO14, Form1.TB_FloodTO15,
                              Form1.TB_FloodTO16, Form1.TB_FloodTO17, Form1.TB_FloodTO18, Form1.TB_FloodTO19, Form1.TB_FloodTO20, Form1.TB_FloodTO21, Form1.TB_FloodTO22, Form1.TB_FloodTO23,
                              Form1.TB_FloodTO24, Form1.TB_FloodTO25, Form1.TB_FloodTO26, Form1.TB_FloodTO27, Form1.TB_FloodTO28, Form1.TB_FloodTO29, Form1.TB_FloodTO30}


End Class
