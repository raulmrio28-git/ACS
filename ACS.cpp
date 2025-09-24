#define _CRT_SECURE_NO_WARNINGS
#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <sstream>
#include <optional>
#include <algorithm>
#include <cctype>
#include <cstdlib>
#include <fstream>

typedef enum {
    ACS_NONE = 0,
    ACS_NUMBER,
    ACS_STRING,
    ACS_FLOAT,
    ACS_BOOLEAN
}scr_types;

struct scr_value {
    scr_types type;
    void* value;
    int size;
};

struct scr_function {
	scr_types returnType;
    std::vector<std::string> body;
    std::map<std::string, scr_value> inputParms;
};

class ScriptLauncher {  
private:
    //Header of file........

    std::string includeDef = "@Include";

    //Functions...

    std::string retType = " -> ";
    std::string funDef = "Fun ";
    std::string printDef = "Print";
	std::string executeDef = "Execute";
    std::string getvalDef = "GetValue";
    std::string returnDef = "Return ";

    //Values...
    std::string noneDef = "None"; //please note: must forcefully be capital/sentence case
    std::string trueDef = "true";
    std::string falseDef = "false";

#define ACS_FORMATTER_LEN 2 //%(type)

    std::map<std::string, scr_value> variables;
    std::map<std::string, scr_value> fvariables;
    std::map<std::string, scr_function> functions;
    std::vector<std::string> includes;

    std::vector<std::string> functionCallBlacklist = {
        "if",
		"for",
        "while",
		printDef,
        executeDef,
        getvalDef
	};

    std::string strlower(std::string str) {
        std::string res = str;
        std::transform(res.begin(), res.end(), res.begin(), ::tolower);
        return res;
	}

    scr_types determine_type(std::string value)
    {
        std::string lwr = strlower(value);
		if (value == noneDef) return ACS_NONE;
        if (value.size() >= 2 && value.front() == '"' && value.back() == '"') return ACS_STRING;
        if (lwr == trueDef || lwr == falseDef) return ACS_BOOLEAN;
        bool isFloat = false;
        for (char c : value) {
            if (c == '.') {
                if (isFloat) return ACS_STRING; // second dot, not a float
                isFloat = true;
            } else if (!isdigit(c)) {
                return ACS_STRING; // not a digit, not a float
            }
        }
		return isFloat ? ACS_FLOAT : ACS_NUMBER;
    }

    std::string trim(const std::string& str) {
        size_t start = str.find_first_not_of(" \t\n\r");
        size_t end = str.find_last_not_of(" \t\n\r");
        if (start == std::string::npos) return "";
        return str.substr(start, end - start + 1);
    }

    bool isCommentOrEmpty(const std::string& line) {
        std::string trimmed = trim(line);
        return trimmed.empty() || trimmed[0] == '#';
    }

    bool checkIfPtrIsValid(void* p)
    {
		//Quick way to check for some common invalid pointers
        bool p1 = (((unsigned int)p) & 0xffffffff) != 0xcccccccc;
		bool p2 = (((unsigned int)p) & 0xffffffff) != 0x00000000;
        bool p3 = (((unsigned int)p) & 0xffffffff) != 0xcdcdcdcd;
        bool p4 = (((unsigned int)p) & 0xffffffff) != 0xdddddddd;
		return p1 && p2 && p3 && p4;
    }

    void initValueEx(scr_value& sval, std::string value, scr_types type)
    { 
		sval.type = type;
        switch (type) {
            case ACS_STRING: {
                std::string* str = new std::string(value);
                if (sval.value && checkIfPtrIsValid(sval.value)) //clean up old value
					delete static_cast<std::string*>(sval.value);
                sval.value = str;
                sval.size = static_cast<int>(str->size());
                break;
            }
            case ACS_NUMBER: {
                int* num = new int(std::stoi(value));
                if (sval.value && checkIfPtrIsValid(sval.value)) //clean up old value
					delete static_cast<int*>(sval.value);
                sval.value = num;
                sval.size = sizeof(int);
                break;
            }
            case ACS_FLOAT: {
                float* f = new float(std::stof(value));
				if (sval.value && checkIfPtrIsValid(sval.value)) //clean up old value
					delete static_cast<float*>(sval.value);
                sval.value = f;
                sval.size = sizeof(float);
                break;
            }
            case ACS_BOOLEAN: {
                bool* b = new bool(strlower(value) == trueDef);
				if (sval.value && checkIfPtrIsValid(sval.value)) //clean up old value
					delete static_cast<bool*>(sval.value);
                sval.value = b;
                sval.size = sizeof(bool);
                break;
            }
        }
    }

    void initValue(scr_value& sval, std::string value)
    {
        scr_types type = determine_type(value);
		sval.size = 0;
        if (type == ACS_STRING) //remove quotes
        {
            std::string temp = value;
            if (temp.size() >= 2 && temp[0] == '"' && temp[temp.size() - 1] == '"')
                temp = temp.substr(1, temp.size() - 2);
            initValueEx(sval, temp, type);
        }
        else
            initValueEx(sval, value, type);
    }

    scr_types getValueType(std::string type)
    {
        if (type == "String") return ACS_STRING;
        else if (type == "Number") return ACS_NUMBER;
        else if (type == "Float") return ACS_FLOAT;
        else if (type == "Boolean") return ACS_BOOLEAN;
        else return ACS_NONE;
    }

    std::string setValueType(scr_types type)
    {
        switch (type) {
        case ACS_STRING: return "String";
        case ACS_NUMBER: return "Number";
        case ACS_FLOAT: return "Float";
        case ACS_BOOLEAN: return "Boolean";
        default: return "None";
        }
	}

    bool parseVariable(const std::string& line, std::map<std::string, scr_value> &vars) {
        size_t eqPos = line.find('=');
        if (eqPos == std::string::npos) return false;
        std::optional<scr_value> sval;

        std::string leftSide = trim(line.substr(0, eqPos));
        std::string rightSide = trim(line.substr(eqPos + 1));

        size_t firstSpace = leftSide.find_first_of(" \t");
        if (firstSpace == std::string::npos) return false;

        std::string typeStr = trim(leftSide.substr(0, firstSpace));
        std::string varName = trim(leftSide.substr(firstSpace + 1));

        scr_types varType = getValueType(typeStr);
        if (varType == ACS_NONE)
            return false; // Not a recognized type

        std::string varValue = rightSide;
        if (varValue.empty()) return false;
        if (varValue.find(getvalDef + "(") != std::string::npos)
        {
            sval = executeValue(varValue, "");
			if (!sval.has_value()) return false;
            if (sval->type != varType) return false; // Type mismatch
            {
                vars[varName] = *sval;
				return true;
            }
        }
        else
        {
            scr_value sval2;
            if (varValue.size() >= 2 && varValue[0] == '"' && varValue[varValue.size() - 1] == '"') {
                varValue = varValue.substr(1, varValue.size() - 2);
            }
            else if (varValue == noneDef) {
                varValue = "";
            }

            sval2.type = varType;
            sval2.size = 0;
            initValueEx(sval2, varValue, varType);
            vars[varName] = sval2;
            return true;
        }
		return false;
    }

    bool parseFunction(const std::string& line, std::vector<std::string>& scriptLines,
        size_t& currentLine, std::string& currentFunction) {
        if (line.find(funDef) != 0) return false;

        size_t parenStart = line.find('(');
        size_t parenEnd = line.find(')');

		size_t hasReturnType = line.find(retType);

        scr_types returnType = ACS_NONE;

        if (parenStart == std::string::npos || parenEnd == std::string::npos) return false;

        std::string funcName = trim(line.substr(funDef.length(), parenStart - funDef.length()));
        currentFunction = funcName;

        //parse params
        std::string paramList = trim(line.substr(parenStart + 1, parenEnd - parenStart - 1));
        std::map<std::string, scr_value> inputParms;
        if (!paramList.empty() && paramList != "None") {
            std::istringstream iss(paramList);
            std::string param;
            while (std::getline(iss, param, ',')) {
                param = trim(param);
                size_t space = param.find(' ');
                if (space != std::string::npos) {
                    std::string typeStr = trim(param.substr(0, space));
                    std::string nameStr = trim(param.substr(space + 1));
                    scr_types t = getValueType(typeStr);
                    scr_value v;
                    v.type = t;
                    v.value = nullptr;
                    v.size = 0;
                    inputParms[nameStr] = v;
                }
            }
        }

        currentLine++;
        if (currentLine >= scriptLines.size() || trim(scriptLines[currentLine]) != "{") {
            return false;
        }

        currentLine++;
        std::vector<std::string> functionBody;

        while (currentLine < scriptLines.size()) {
            std::string bodyLine = trim(scriptLines[currentLine]);
            if (scriptLines[currentLine] == "}") { //must not check bodyLine because of indents
                break;
            }
            if (!bodyLine.empty()) {
                functionBody.push_back(bodyLine);
            }
            currentLine++;
        }

        if (hasReturnType != std::string::npos)
        {
            std::string retTypeStr = trim(line.substr(hasReturnType + retType.length(), parenStart - (hasReturnType + retType.length())));
			returnType = getValueType(retTypeStr);
        }

		scr_function function;
        function.returnType = returnType; // Default return type
        function.body = functionBody;
        function.inputParms = inputParms;

        functions[funcName] = function;
        return true;
    }

    bool parseReturn(const std::string& line) {
        if (line.find(returnDef) != 0) return false;
        std::string returnValue = trim(line.substr(returnDef.length()));
		size_t is_operation = line.find_first_of("+-*/");

        if(is_operation != std::string::npos)
        {
            // we have an operation, so we need to parse it differently
            return parseVarOperation(returnValue, "__return__", fvariables);
		}

        if (returnValue == noneDef) {
            variables["__return__"].type = ACS_NONE;
        }
        else {
            initValue(variables["__return__"], returnValue);
        }
        return true;
    }

    bool parseInclude(const std::string& line) {
        if (line.find(includeDef) != 0) return false;

        size_t startParen = line.find('(');
        size_t endParen = line.find(')');
        if (startParen == std::string::npos || endParen == std::string::npos) return false;

        std::string includeName = trim(line.substr(startParen + 1, endParen - startParen - 1));
        includes.push_back(includeName);
        return true;
    }

    bool parseFunctionCall(const std::string& line, std::map<std::string, scr_value>& vars) {
        // Type var = Func(args)
        size_t eqPos = line.find('=');
        std::string left, right;
        if (eqPos != std::string::npos) {
            left = trim(line.substr(0, eqPos));
            right = trim(line.substr(eqPos + 1));
        }
        else {
            right = trim(line);
        }

        // find: FuncName(args)

		// we need to blacklist the following: if, for, while, internal functions like Print
        for (const auto& blk : functionCallBlacklist) {
            if (right.find(blk + "(") == 0) return false;
		}

        size_t parenStart = right.find('(');
        size_t parenEnd = right.find(')');
        if (parenStart == std::string::npos || parenEnd == std::string::npos) return false;

        std::string funcName = trim(right.substr(0, parenStart));
        std::string argList = trim(right.substr(parenStart + 1, parenEnd - parenStart - 1));

        std::vector<std::string> args;
        if (!argList.empty() && argList != "None") {
            std::istringstream iss(argList);
            std::string arg;
            while (std::getline(iss, arg, ',')) {
                args.push_back(trim(arg));
            }
        }


        auto result = executeFunction(funcName, args, vars);

        if (eqPos != std::string::npos && !left.empty() && result.has_value()) {
            size_t firstSpace = left.find_first_of(" \t");
            if (firstSpace == std::string::npos) return false;
            std::string typeStr = trim(left.substr(0, firstSpace));
            std::string varName = trim(left.substr(firstSpace + 1));
            scr_types varType = getValueType(typeStr);
            if (varType == ACS_NONE) return false;

            scr_value sval;
            sval.type = result->type;
            sval.size = result->size;
            switch (result->type) {
            case ACS_STRING:
                sval.value = new std::string(*(static_cast<std::string*>(result->value)));
                break;
            case ACS_NUMBER:
                sval.value = new int(*(static_cast<int*>(result->value)));
                break;
            case ACS_FLOAT:
                sval.value = new float(*(static_cast<float*>(result->value)));
                break;
            case ACS_BOOLEAN:
                sval.value = new bool(*(static_cast<bool*>(result->value)));
                break;
            default:
                sval.value = nullptr;
            }
            vars[varName] = sval;
            return true;
        }

        if (eqPos == std::string::npos && result.has_value()) {
            return true;
        }

        return false;
    }

    void executePrint(const std::string& line) {
        size_t startQuote = line.find('"');
        size_t endQuote = line.rfind('"');
		std::vector<std::string> args;
        if (startQuote == std::string::npos || endQuote == std::string::npos) return;

        std::string formatStr = line.substr(startQuote + 1, endQuote - startQuote - 1);

		size_t argStart = line.find(',', endQuote);
		while (argStart != std::string::npos) {
			argStart++;
			size_t argEnd = line.find(',', argStart);
			if (argEnd == std::string::npos) argEnd = line.find(')', argStart);
			if (argEnd == std::string::npos) argEnd = line.length();
			std::string arg = trim(line.substr(argStart, argEnd - argStart));
			if (!arg.empty()) {
				args.push_back(arg);
			}
			argStart = line.find(',', argEnd);
		}

		for (const auto& arg : args) {
			scr_value value;
			if (fvariables.find(arg) != fvariables.end()) {
				value = fvariables[arg];
			} else if (variables.find(arg) != variables.end()) {
				value = variables[arg];
			} else {
                initValue(value, arg);
			}
			size_t placeholder = formatStr.find("%s");
			if (placeholder != std::string::npos && value.type == ACS_STRING) {
				formatStr.replace(placeholder, ACS_FORMATTER_LEN, *(static_cast<std::string*>(value.value)));
				continue;
			}
			size_t placeholder_d = formatStr.find("%d");
			if (placeholder_d != std::string::npos && value.type == ACS_NUMBER) {
				formatStr.replace(placeholder_d, ACS_FORMATTER_LEN, std::to_string(*(static_cast<int*>(value.value))));
				continue;
			}
			size_t placeholder_f = formatStr.find("%f");
			if (placeholder_f != std::string::npos && value.type == ACS_FLOAT) {
				formatStr.replace(placeholder_f, ACS_FORMATTER_LEN, std::to_string(*(static_cast<float*>(value.value))));
				continue;
			}
			size_t placeholder_b = formatStr.find("%b");
			if (placeholder_b != std::string::npos && value.type == ACS_BOOLEAN) {
				formatStr.replace(placeholder_b, ACS_FORMATTER_LEN, (*(static_cast<bool*>(value.value))) ? "True" : "False");
				continue;
			}
		}

        std::cout << formatStr << std::endl;
    }

    void executeBlock(const std::vector<std::string>& lines, size_t& i) {
        std::vector<std::string> block;
        int braceCount = 0;
        i++; // skip the opening brace
        for (; i < lines.size(); ++i) {
            std::string line = trim(lines[i]);
            if (line == "{") {
                braceCount++;
                block.push_back(line);
            } else if (line == "}") {
                if (braceCount == 0) break;
                braceCount--;
                block.push_back(line);
            } else {
                block.push_back(line);
            }
        }

        for (size_t j = 0; j < block.size(); ++j) {
            std::string line = trim(block[j]);
            if (line.find("if(") == 0) {
                executeIf(block, j);
                continue;
            }
            if (line.find("for(") == 0) { executeFor(block, i); continue; }
            if (line.find("while(") == 0) { executeWhile(block, i); continue; }
            if (line.find(printDef + "(") == 0) { executePrint(line); continue; }
            if (line.find(executeDef +"(") == 0) { executeSystem(line); continue; }
            //find if var attrib
            for (const auto& fvar : fvariables) {
                if (line.find(fvar.first) != std::string::npos) {
                    if (parseVarAttrib(line, fvar.first, fvariables)) {
                        continue;
                    }
                }
            }
            for (const auto& var : variables) {
                if (line.find(var.first) != std::string::npos) {
                    if (parseVarAttrib(line, var.first, variables)) {
                        continue;
                    }
                }
            }
            if (parseReturn(line)) break;
            if (isCommentOrEmpty(line)) continue;
            if (parseFunctionCall(line, fvariables)) continue;
            if (parseVariable(line, fvariables)) continue;
            if (line == "{") executeBlock(block, j);
        }
    }

    bool evalCondition(const std::string& cond) {
        std::string c = trim(cond);
        if (strlower(c) == trueDef) return true;
        if (strlower(c) == falseDef) return false;
        if (fvariables.count(c)) {
            scr_value v = fvariables[c];
            if (v.type == ACS_BOOLEAN) return *(static_cast<bool*>(v.value));
        }
        if (variables.count(c)) {
            scr_value v = variables[c];
            if (v.type == ACS_BOOLEAN) return *(static_cast<bool*>(v.value));
        }
        size_t eq = c.find("==");
        if (eq != std::string::npos) {
            std::string left = trim(c.substr(0, eq));
            std::string right = trim(c.substr(eq + 2));
            scr_value vl, vr;
            scr_types vlt, vrt;

            vlt = determine_type(left);
            vrt = determine_type(right);
            if (right.size() >= 2 && right.front() == '"' && right.back() == '"') {
                right = right.substr(1, right.size() - 2);
            }

            //left
            if (fvariables.count(left)) {
                vl = fvariables[left];
            }
            else if (variables.count(left)) {
                vl = variables[left];
            }
            else
            {
                if (vlt != ACS_NONE)
                    initValueEx(vl, left, vlt);
                else
					return false;
            }
            //right
            if (fvariables.count(right)) {
                vr = fvariables[right];
            }
            else if (variables.count(right)) {
                vr = variables[right];
            }
            else
            {
                if (vrt != ACS_NONE)
                    initValueEx(vr, right, vrt);
                else
                    return false;
            }
			if (vl.type != vr.type) return false;
            switch (vl.type) {
            case ACS_STRING:
                return *(static_cast<std::string*>(vl.value)) == *(static_cast<std::string*>(vr.value));
            case ACS_NUMBER:
                return *(static_cast<int*>(vl.value)) == *(static_cast<int*>(vr.value));
            case ACS_FLOAT:
                return *(static_cast<float*>(vl.value)) == *(static_cast<float*>(vr.value));
            case ACS_BOOLEAN:
                return *(static_cast<bool*>(vl.value)) == *(static_cast<bool*>(vr.value));
            default:
                return false;
			}
        }
        eq = c.find("!=");
        if (eq != std::string::npos) {
            std::string left = trim(c.substr(0, eq));
            std::string right = trim(c.substr(eq + 2));
            scr_value vl, vr;
            scr_types vlt, vrt;

            vlt = determine_type(left);
            vrt = determine_type(right);
            if (right.size() >= 2 && right.front() == '"' && right.back() == '"') {
                right = right.substr(1, right.size() - 2);
            }

            //left
            if (fvariables.count(left)) {
                vl = fvariables[left];
            }
            else if (variables.count(left)) {
                vl = variables[left];
            }
            else
            {
                if (vlt != ACS_NONE)
                    initValueEx(vl, left, vlt);
                else
                    return false;
            }
            //right
            if (fvariables.count(right)) {
                vr = fvariables[right];
            }
            else if (variables.count(right)) {
                vr = variables[right];
            }
            else
            {
                if (vrt != ACS_NONE)
                    initValueEx(vr, right, vrt);
                else
                    return false;
            }
            if (vl.type != vr.type) return false;
            switch (vl.type) {
            case ACS_STRING:
                return *(static_cast<std::string*>(vl.value)) != *(static_cast<std::string*>(vr.value));
            case ACS_NUMBER:
                return *(static_cast<int*>(vl.value)) != *(static_cast<int*>(vr.value));
            case ACS_FLOAT:
                return *(static_cast<float*>(vl.value)) != *(static_cast<float*>(vr.value));
            case ACS_BOOLEAN:
                return *(static_cast<bool*>(vl.value)) != *(static_cast<bool*>(vr.value));
            default:
                return false;
            }
		}
        return false;
    }

    void findEndOfIf(const std::vector<std::string>& lines, size_t& i) {
        size_t blockEnd = i;
        int braceCount = 0;
        while (blockEnd < lines.size()) {
            std::string l = trim(lines[blockEnd]);
			//must stop if line is blank, comment or is another if/for/while
			size_t findif = l.find("if(");
			size_t findfor = l.find("for(");
			size_t findwhile = l.find("while(");
			bool is_if = (findif != std::string::npos && findif == 0);
			bool is_for = (findfor != std::string::npos && findfor == 0);
			bool is_while = (findwhile != std::string::npos && findwhile == 0);
            bool is_var = false;

			//also skip variable attribs, returns, function calls, variable declarations, print statements
			//we only want to find the end of the if/elseif/else chain
            //vars
            {
                size_t find_num = l.find("Number ");
				size_t find_str = l.find("String ");
				size_t find_flt = l.find("Float ");
				size_t find_bol = l.find("Boolean ");
                is_var = (find_num != std::string::npos && find_num == 0) ||
                    (find_str != std::string::npos && find_str == 0) ||
                    (find_flt != std::string::npos && find_flt == 0) ||
                    (find_bol != std::string::npos && find_bol == 0);
            }
            //return
			bool is_return = (l.find(returnDef) == 0);
			//function call - internals
			bool is_func_call = false;
			for (const auto& blk : functionCallBlacklist) {
				if (l.find(blk + "(") == 0) {
					is_func_call = true;
					break;
				}
			}
            //function call - script-wide
			bool is_func_call2 = false;
			for (const auto& func : functions) {
				if (l.find(func.first + "(") == 0) {
					is_func_call2 = true;
					break;
				}
			}
			//variable attrib (var = var + 1)...
			bool is_var_attrib = false;
            for (const auto& fvar : fvariables) {
                if (l.find(fvar.first) != std::string::npos) {
                    if (parseVarAttrib(l, fvar.first, fvariables)) {
                        is_var_attrib = true;
                        break;
                    }
                }
            }

            if (isCommentOrEmpty(l) || is_if || is_for || is_while
             || is_var || is_return || is_func_call || is_func_call2 || is_var_attrib)
				break;
            if (l.find("elseif") == 0 || l.find("else") == 0) {
                int localBraceCount = 0;
                size_t localBlockStart = blockEnd;
                while (localBlockStart < lines.size() && trim(lines[localBlockStart]).find("{") != std::string::npos)
			        localBlockStart++;
                if (localBlockStart >= lines.size()) break;
                // Find closing brace for this block
                size_t localBlockEnd = localBlockStart;
                for (++localBlockEnd; localBlockEnd < lines.size(); ++localBlockEnd) {
                    std::string ll = trim(lines[localBlockEnd]);
                    if (ll == "{") localBraceCount++;
                    else if (ll == "}") {
                        if (localBraceCount == 0) break;
                        localBraceCount--;
                    }
                }
                blockEnd = localBlockEnd + 1;
                continue;
            }
            blockEnd++;
		}
		i = blockEnd-1;
	}

    void executeIf(std::vector<std::string>& lines, size_t& i) {
        std::string line = trim(lines[i]);
        size_t condStart = line.find('(');
        size_t condEnd = line.find(')');
        size_t blockStart = i + 1;
        size_t brace_start = line.find('{');

        if (condStart == std::string::npos || condEnd == std::string::npos) return;
        std::string cond = line.substr(condStart + 1, condEnd - condStart - 1);

        // Find the start of the if block
        if (brace_start == std::string::npos) {
            while (blockStart < lines.size() && trim(lines[blockStart]) != "{") blockStart++;
            if (blockStart >= lines.size()) return;
        } else {
            size_t bracePos = lines[i].find('{');
            if (bracePos != std::string::npos) {
                lines[i].erase(bracePos, 1);
                lines.insert(lines.begin() + i + 1, "{");
            }
        }

        // Find the end of the if block
        size_t blockEnd = blockStart;
        int braceCount = 0;
        for (++blockEnd; blockEnd < lines.size(); ++blockEnd) {
            std::string l = trim(lines[blockEnd]);
            if (l == "{") braceCount++;
            else if (l == "}") {
                if (braceCount == 0) break;
                braceCount--;
            }
        }

        // If condition is true, execute and skip the rest
        if (evalCondition(cond)) {
            executeBlock(lines, blockStart);
            i = blockEnd; // Move i to after the if/elseif/else chain
			findEndOfIf(lines, i); // Move i to the end of the entire if/elseif/else chain
            return;
        }

        // Now check for elseif/else
        size_t searchIdx = blockEnd;
        while (searchIdx + 1 < lines.size()) {
            std::string nextLine = trim(lines[searchIdx + 1]);
            if (nextLine.find("elseif") == 0) {
                size_t econdStart = nextLine.find('(');
                size_t econdEnd = nextLine.find(')');
                size_t eblockStart = searchIdx + 2;
                size_t ebrace_start = nextLine.find('{');

                if (econdStart == std::string::npos || econdEnd == std::string::npos) break;
                std::string econd = nextLine.substr(econdStart + 1, econdEnd - econdStart - 1);

                if (ebrace_start == std::string::npos) {
                    while (eblockStart < lines.size() && trim(lines[eblockStart]) != "{") eblockStart++;
                    if (eblockStart >= lines.size()) break;
                } else {
                    size_t bracePos = lines[searchIdx + 1].find('{');
                    if (bracePos != std::string::npos) {
                        lines[searchIdx + 1].erase(bracePos, 1);
                        lines.insert(lines.begin() + searchIdx + 2, "{");
                    }
                }

                size_t eblockEnd = eblockStart;
                int ebraceCount = 0;
                for (++eblockEnd; eblockEnd < lines.size(); ++eblockEnd) {
                    std::string l = trim(lines[eblockEnd]);
                    if (l == "{") ebraceCount++;
                    else if (l == "}") {
                        if (ebraceCount == 0) break;
                        ebraceCount--;
                    }
                }

                if (evalCondition(econd)) {
                    executeBlock(lines, eblockStart);
                    i = eblockEnd; // Move i to after the if/elseif/else chain
                    findEndOfIf(lines, i); // Move i to the end of the entire if/elseif/else chain
                    return;
                } else {
                    searchIdx = eblockEnd;
                }
            } else if (nextLine.find("else") == 0 && nextLine != "elseif") {
                size_t elseBlockStart = searchIdx + 2;
                size_t elseBraceStart = nextLine.find('{');

                if (elseBraceStart == std::string::npos) {
                    while (elseBlockStart < lines.size() && trim(lines[elseBlockStart]) != "{") elseBlockStart++;
                    if (elseBlockStart >= lines.size()) break;
                } else {
                    size_t bracePos = lines[searchIdx + 1].find('{');
                    if (bracePos != std::string::npos) {
                        lines[searchIdx + 1].erase(bracePos, 1);
                        lines.insert(lines.begin() + searchIdx + 2, "{");
                    }
                }

                size_t elseBlockEnd = elseBlockStart;
                int elseBraceCount = 0;
                for (++elseBlockEnd; elseBlockEnd < lines.size(); ++elseBlockEnd) {
                    std::string l = trim(lines[elseBlockEnd]);
                    if (l == "{") elseBraceCount++;
                    else if (l == "}") {
                        if (elseBraceCount == 0) break;
                        elseBraceCount--;
                    }
                }

                executeBlock(lines, elseBlockStart);
                i = elseBlockEnd; // Move i to after the if/elseif/else chain
                findEndOfIf(lines, i); // Move i to the end of the entire if/elseif/else chain
                return;
            } else {
                break;
            }
        }
        // If no branch matched, move i to after the if/elseif/else chain
        i = searchIdx;
}

    void executeFor(std::vector<std::string>& lines, size_t& i) {
        std::string line = trim(lines[i]);
        size_t parenStart = line.find('(');
        size_t parenEnd = line.find(')');
        if (parenStart == std::string::npos || parenEnd == std::string::npos) return;

        std::string forContent = line.substr(parenStart + 1, parenEnd - parenStart - 1);
        // var = start to end; increment
        size_t eqPos = forContent.find('=');
        size_t toPos = forContent.find("to");
        size_t semiPos = forContent.find(';');
        if (eqPos == std::string::npos || toPos == std::string::npos || semiPos == std::string::npos) return;

        std::string varName = trim(forContent.substr(0, eqPos));
        std::string startVal = trim(forContent.substr(eqPos + 1, toPos - eqPos - 1));
        std::string endVal = trim(forContent.substr(toPos + 2, semiPos - toPos - 2));
        std::string incrementVal = trim(forContent.substr(semiPos + 1));

        // Determine type (Number or Float)
        scr_value& var = fvariables[varName];
        if (startVal.find('.') != std::string::npos || endVal.find('.') != std::string::npos || incrementVal.find('.') != std::string::npos) {
            float start = std::stof(startVal);
            float end = std::stof(endVal);
            float inc = std::stof(incrementVal);
            var.type = ACS_FLOAT;
            var.value = new float(start);
            var.size = sizeof(float);

            size_t blockStart = i + 1;
            size_t brace_start = line.find('{');
            if (brace_start == std::string::npos)
            {
                while (blockStart < lines.size() && trim(lines[blockStart]) != "{") blockStart++;
                if (blockStart >= lines.size()) return;
            }
            else
            {
                size_t bracePos = lines[i].find('{');
                if (bracePos != std::string::npos) {
                    lines[i].erase(bracePos, 1);
                    lines.insert(lines.begin() + i + 1, "{");
                }
            }
            size_t blockEnd = blockStart;
            int braceCount = 0;
            for (++blockEnd; blockEnd < lines.size(); ++blockEnd) {
                std::string l = trim(lines[blockEnd]);
                if (l == "{") braceCount++;
                else if (l == "}") {
                    if (braceCount == 0) break;
                    braceCount--;
                }
            }
            std::vector<std::string> block(lines.begin() + blockStart + 1, lines.begin() + blockEnd);

            for (float v = start; inc > 0 ? v < end : v > end; v += inc) {
                *(static_cast<float*>(var.value)) = v;
                for (size_t j = 0; j < block.size(); ++j) {
                    std::string bline = trim(block[j]);
                    if (bline.find("if(") == 0) { executeIf(block, j); continue; }
                    if (bline.find("for(") == 0) { executeFor(block, j); continue; }
                    if (bline.find("while(") == 0) { executeWhile(block, j); continue; }
                    if (bline.find(printDef + "(") == 0) { executePrint(bline); continue; }
                    if (bline.find(executeDef + "(") == 0) { executeSystem(line); continue; }
                    //find if var attrib
                    for (const auto& fvar : fvariables) {
                        if (bline.find(fvar.first) != std::string::npos) {
                            if (parseVarAttrib(bline, fvar.first, fvariables)) {
                                continue;
                            }
                        }
                    }
                    for (const auto& var : variables) {
                        if (bline.find(var.first) != std::string::npos) {
                            if (parseVarAttrib(bline, var.first, variables)) {
                                continue;
                            }
                        }
                    }
                    if (parseReturn(bline)) return;
                    if (isCommentOrEmpty(bline)) continue;
                    if (parseFunctionCall(bline, fvariables)) continue;
                    if (parseVariable(bline, fvariables)) continue;
                    if (bline == "{") executeBlock(block, j);
                }
            }
            i = blockEnd;
            delete static_cast<float*>(var.value);
            fvariables.erase(varName);
        } else {
            int start = std::stoi(startVal);
            int end = std::stoi(endVal);
            int inc = std::stoi(incrementVal);
            var.type = ACS_NUMBER;
            var.value = new int(start);
            var.size = sizeof(int);

            size_t blockStart = i + 1;
            size_t brace_start = line.find('{');
            if (brace_start == std::string::npos)
            {
                while (blockStart < lines.size() && trim(lines[blockStart]) != "{") blockStart++;
                if (blockStart >= lines.size()) return;
            }
            else
            {
                size_t bracePos = lines[i].find('{');
                if (bracePos != std::string::npos) {
                    lines[i].erase(bracePos, 1);
                    lines.insert(lines.begin() + i + 1, "{");
                }
            }
            size_t blockEnd = blockStart;
            int braceCount = 0;
            for (++blockEnd; blockEnd < lines.size(); ++blockEnd) {
                std::string l = trim(lines[blockEnd]);
                if (l == "{") braceCount++;
                else if (l == "}") {
                    if (braceCount == 0) break;
                    braceCount--;
                }
            }
            std::vector<std::string> block(lines.begin() + blockStart + 1, lines.begin() + blockEnd);

            for (int v = start; inc > 0 ? v < end : v > end; v += inc) {
                *(static_cast<int*>(var.value)) = v;
                for (size_t j = 0; j < block.size(); ++j) {
                    std::string bline = trim(block[j]);
                    if (bline.find("if(") == 0) { executeIf(block, j); continue; }
                    if (bline.find("for(") == 0) { executeFor(block, j); continue; }
                    if (bline.find("while(") == 0) { executeWhile(block, j); continue; }
                    if (bline.find(printDef + "(") == 0) { executePrint(bline); continue; }
                    if (bline.find(executeDef + "(") == 0) { executeSystem(line); continue; }
                    //find if var attrib
                    for (const auto& fvar : fvariables) {
                        if (bline.find(fvar.first) != std::string::npos) {
                            if (parseVarAttrib(bline, fvar.first, fvariables)) {
                                continue;
                            }
                        }
                    }
                    for (const auto& var : variables) {
                        if (bline.find(var.first) != std::string::npos) {
                            if (parseVarAttrib(bline, var.first, variables)) {
                                continue;
                            }
                        }
                    }
                    if (parseReturn(bline)) return;
                    if (isCommentOrEmpty(bline)) continue;
                    if (parseFunctionCall(bline, fvariables)) continue;
                    if (parseVariable(bline, fvariables)) continue;
                    if (bline == "{") executeBlock(block, j);
                }
            }
            i = blockEnd;
            delete static_cast<int*>(var.value);
            fvariables.erase(varName);
        }
    }

    bool parseVarOperation(std::string expr, std::string var_name, std::map<std::string, scr_value>& vars)
    {
        size_t opPos = std::string::npos;
        char op = 0;
        for (size_t j = 0; j < expr.size(); j++)
        {
            if (expr[j] == '+' || expr[j] == '-' || expr[j] == '*' || expr[j] == '/')
            {
                opPos = j;
                op = expr[j];
                break;
            }
        }
        if (opPos == std::string::npos) return false;
        std::string leftOperand = trim(expr.substr(0, opPos));
        std::string rightOperand = trim(expr.substr(opPos + 1));
        scr_value leftVal, rightVal;
        if (vars.find(leftOperand) != vars.end())
        {
            leftVal = vars[leftOperand];
        }
        else if (variables.find(leftOperand) != variables.end())
        {
            leftVal = variables[leftOperand];
        }
        else
        {
            initValue(leftVal, leftOperand);
        }
        if (vars.find(rightOperand) != vars.end())
        {
            rightVal = vars[rightOperand];
        }
        else if (variables.find(rightOperand) != variables.end())
        {
            rightVal = variables[rightOperand];
        }
        else
        {
            initValue(rightVal, rightOperand);
        }
        scr_value result;
        if (leftVal.type == ACS_FLOAT || rightVal.type == ACS_FLOAT)
        {
            float lval = (leftVal.type == ACS_FLOAT) ? *(static_cast<float*>(leftVal.value)) : static_cast<float>(*(static_cast<int*>(leftVal.value)));
            float rval = (rightVal.type == ACS_FLOAT) ? *(static_cast<float*>(rightVal.value)) : static_cast<float>(*(static_cast<int*>(rightVal.value)));
            float res = 0;
            switch (op)
            {
            case '+': res = lval + rval; break;
            case '-': res = lval - rval; break;
            case '*': res = lval * rval; break;
            case '/': if (rval != 0) res = lval / rval; else return false; break;
            default: return false;
            }
            result.type = ACS_FLOAT;
            result.value = new float(res);
            result.size = sizeof(float);
        }
        else if (leftVal.type == ACS_NUMBER && rightVal.type == ACS_NUMBER)
        {
            int lval = *(static_cast<int*>(leftVal.value));
            int rval = *(static_cast<int*>(rightVal.value));
            int res = 0;
            switch (op)
            {
            case '+': res = lval + rval; break;
            case '-': res = lval - rval; break;
            case '*': res = lval * rval; break;
            case '/': if (rval != 0) res = lval / rval; else return false; break;
            default: return false;
            }
            result.type = ACS_NUMBER;
            result.value = new int(res);
            result.size = sizeof(int);
        }
        else if (leftVal.type == ACS_STRING && rightVal.type == ACS_STRING && op == '+')
        {
            // we only support concat and direct attribution
            // if no rightval, set leftval
            std::string lval = *(static_cast<std::string*>(leftVal.value));
            std::string rval = *(static_cast<std::string*>(rightVal.value));
            std::string res = lval + rval;
            result.type = ACS_STRING;
            result.value = new std::string(res);
            result.size = static_cast<int>(res.size());
        }
        else
        {
            return false; // Unsupported operation
        }
        // Clean up
        if (leftVal.type == ACS_STRING) delete static_cast<std::string*>(leftVal.value);
        else if (leftVal.type == ACS_NUMBER) delete static_cast<int*>(leftVal.value);
        else if (leftVal.type == ACS_FLOAT) delete static_cast<float*>(leftVal.value);
        else if (leftVal.type == ACS_BOOLEAN) delete static_cast<bool*>(leftVal.value);
        if (rightVal.type == ACS_STRING) delete static_cast<std::string*>(rightVal.value);
        else if (rightVal.type == ACS_NUMBER) delete static_cast<int*>(rightVal.value);
        else if (rightVal.type == ACS_FLOAT) delete static_cast<float*>(rightVal.value);
        else if (rightVal.type == ACS_BOOLEAN) delete static_cast<bool*>(rightVal.value);
        // Assign result to variable
        if (vars.find(var_name) != vars.end())
        {
            scr_value& var = vars[var_name];
            if (var.value != leftVal.value && var.value != rightVal.value) // avoid double free
            {
                if (var.type == ACS_STRING) delete static_cast<std::string*>(var.value);
                else if (var.type == ACS_NUMBER) delete static_cast<int*>(var.value);
                else if (var.type == ACS_FLOAT) delete static_cast<float*>(var.value);
                else if (var.type == ACS_BOOLEAN) delete static_cast<bool*>(var.value);
            }
        }
        if (var_name == "__return__")
			variables["__return__"] = result;
        else
            vars[var_name] = result;
        return true;
    }

    bool parseVarAttrib(std::string line, std::string var_name, std::map<std::string, scr_value> &vars)
    {
        //formats:
		// var = (var/var2) +|-|*|/ (var/var2/number/float)

        size_t varPos = line.find(var_name);
        if (varPos == std::string::npos) return false;
        size_t eqPos = line.find('=', varPos + var_name.size());
        if (eqPos == std::string::npos) return false;
        std::string rightSide = trim(line.substr(eqPos + 1));
		if (rightSide.find_first_of("+-/*") != std::string::npos)
            return parseVarOperation(rightSide, var_name, vars);
        else
        {
            //get type of left
			scr_value v;
            std::optional<scr_value> rv;
            bool f_ex = false;
			if (vars.find(var_name) != vars.end())
				v = vars[var_name];
			else if (variables.find(var_name) != variables.end())
				v = variables[var_name];
            if (rightSide.find(getvalDef + "(") != std::string::npos)
                rv = executeValue(rightSide, setValueType(v.type));
            //assign
            if (rv.has_value())
            {
                if (rv.value().type != ACS_NONE)
                {
                    if (rv.value().type == v.type)
                    {
                        if (v.type == ACS_STRING)
                        {
                            delete static_cast<std::string*>(v.value);
							v.value = new std::string(*(static_cast<std::string*>(rv.value().value)));
                            v.size = static_cast<int>(static_cast<std::string*>(v.value)->size());
                        }
                        else if (v.type == ACS_NUMBER)
                        {
							delete static_cast<int*>(v.value);
                            v.value = new int(*(static_cast<int*>(rv.value().value)));
                            v.size = sizeof(int);
                        }
                        else if (v.type == ACS_FLOAT)
						{
                            delete static_cast<float*>(v.value);
                            v.value = new float(*(static_cast<float*>(rv.value().value)));
                            v.size = sizeof(float);
                        }
                        else if (v.type == ACS_BOOLEAN)
						{
                            delete static_cast<bool*>(v.value);
                            v.value = new bool(*(static_cast<bool*>(rv.value().value)));
                            v.size = sizeof(bool);
                        }
                        if (var_name == "__return__")
                            variables["__return__"] = v;
                        else
                            vars[var_name] = v;
                        return true;
                    }
                    else
                    {
						return false;
                    }
                }
                else
                {
                    return false;
				}
            }
            else
            {
                scr_types rtype = determine_type(rightSide);
                if (rtype == ACS_NONE) return false;
                if (rtype != v.type) return false;
                initValueEx(v, rightSide, rtype);
                if (var_name == "__return__")
                    variables["__return__"] = v;
                else
                    vars[var_name] = v;
                return true;
            }
        }
    }

    void executeWhile(std::vector<std::string>& lines, size_t& i) {
        std::string line = trim(lines[i]);
        size_t parenStart = line.find('(');
        size_t parenEnd = line.find(')');
        if (parenStart == std::string::npos || parenEnd == std::string::npos) return;

        std::string cond = line.substr(parenStart + 1, parenEnd - parenStart - 1);

        size_t blockStart = i + 1;
        size_t brace_start = line.find('{');
        if (brace_start == std::string::npos)
        {
            while (blockStart < lines.size() && trim(lines[blockStart]) != "{") blockStart++;
            if (blockStart >= lines.size()) return;
        }
        else
        {
            size_t bracePos = lines[i].find('{');
            if (bracePos != std::string::npos) {
                lines[i].erase(bracePos, 1);
                lines.insert(lines.begin() + i + 1, "{");
            }
        }
        size_t blockEnd = blockStart;
        int braceCount = 0;
        for (++blockEnd; blockEnd < lines.size(); ++blockEnd) {
            std::string l = trim(lines[blockEnd]);
            if (l == "{") braceCount++;
            else if (l == "}") {
                if (braceCount == 0) break;
                braceCount--;
            }
        }
        std::vector<std::string> block(lines.begin() + blockStart + 1, lines.begin() + blockEnd);

        while (evalWhileCondition(cond)) {
            for (size_t j = 0; j < block.size(); ++j) {
                std::string bline = trim(block[j]);
                if (bline.find("if(") == 0) { executeIf(block, j); continue; }
                if (bline.find("for(") == 0) { executeFor(block, j); continue; }
                if (bline.find("while(") == 0) { executeWhile(block, j); continue; }
                if (bline.find(printDef + "(") == 0) { executePrint(bline); continue; }
                if (bline.find(executeDef + "(") == 0) { executeSystem(line); continue; }
                //find if var attrib
                for (const auto& fvar : fvariables) {
                    if (bline.find(fvar.first) != std::string::npos) {
                        if (parseVarAttrib(bline, fvar.first, fvariables)) {
                            continue;
						}
                    }
                }
                for (const auto& var : variables) {
                    if (bline.find(var.first) != std::string::npos) {
                        if (parseVarAttrib(bline, var.first, variables)) {
                            continue;
                        }
                    }
                }
                if (parseReturn(bline)) return;
                if (isCommentOrEmpty(bline)) continue;
                if (parseFunctionCall(bline, fvariables)) continue;
                if (parseVariable(bline, fvariables)) continue;
                if (bline == "{") executeBlock(block, j);
            }
        }
        i = blockEnd;
    }

    void executeSystem(const std::string& line)
    {
        //takes 2 args: command, parameters
		//can be either script variables or direct strings
        size_t startQuote = line.find('(');
        size_t endQuote = line.rfind(')');
        std::string cmd;
        std::string arg;

		std::string content = line.substr(startQuote + 1, endQuote - startQuote - 1);
		// Split by first comma
		size_t commaPos = content.find(',');
		if (commaPos != std::string::npos) {
			cmd = trim(content.substr(0, commaPos));
			arg = trim(content.substr(commaPos + 1));
		}
        else {
            cmd = trim(content);
            arg = "";
        }
        if (cmd.size() >= 2 && cmd.front() == '"' && cmd.back() == '"') {
            cmd = cmd.substr(1, cmd.size() - 2);
        }
        else if (fvariables.count(cmd)) {
            scr_value v = fvariables[cmd];
            if (v.type == ACS_STRING) cmd = *(static_cast<std::string*>(v.value));
            else if (v.type == ACS_NUMBER) cmd = std::to_string(*(static_cast<int*>(v.value)));
            else if (v.type == ACS_FLOAT) cmd = std::to_string(*(static_cast<float*>(v.value)));
            else if (v.type == ACS_BOOLEAN) cmd = (*(static_cast<bool*>(v.value))) ? "true" : "false";
        }
        else if (variables.count(cmd)) {
            scr_value v = variables[cmd];
            if (v.type == ACS_STRING) cmd = *(static_cast<std::string*>(v.value));
            else if (v.type == ACS_NUMBER) cmd = std::to_string(*(static_cast<int*>(v.value)));
            else if (v.type == ACS_FLOAT) cmd = std::to_string(*(static_cast<float*>(v.value)));
            else if (v.type == ACS_BOOLEAN) cmd = (*(static_cast<bool*>(v.value))) ? "true" : "false";
		}
        if (arg.size() >= 2 && arg.front() == '"' && arg.back() == '"') {
            arg = arg.substr(1, arg.size() - 2);
        }
        else if (fvariables.count(arg)) {
            scr_value v = fvariables[arg];
            if (v.type == ACS_STRING) arg = *(static_cast<std::string*>(v.value));
            else if (v.type == ACS_NUMBER) arg = std::to_string(*(static_cast<int*>(v.value)));
            else if (v.type == ACS_FLOAT) arg = std::to_string(*(static_cast<float*>(v.value)));
            else if (v.type == ACS_BOOLEAN) arg = (*(static_cast<bool*>(v.value))) ? "true" : "false";
        }
        else if (variables.count(arg)) {
            scr_value v = variables[arg];
            if (v.type == ACS_STRING) arg = *(static_cast<std::string*>(v.value));
            else if (v.type == ACS_NUMBER) arg = std::to_string(*(static_cast<int*>(v.value)));
            else if (v.type == ACS_FLOAT) arg = std::to_string(*(static_cast<float*>(v.value)));
            else if (v.type == ACS_BOOLEAN) arg = (*(static_cast<bool*>(v.value))) ? "true" : "false";
        }
        {
			char* fullCmd = new char[cmd.size() + arg.size() + 2];
			if (arg.empty())
				sprintf_s(fullCmd, cmd.size() + arg.size() + 2, "%s", cmd.c_str());
			else
				sprintf_s(fullCmd, cmd.size() + arg.size() + 2, "%s %s", cmd.c_str(), arg.c_str());
			std::system(fullCmd);
			delete[] fullCmd;
        }
    }

    bool evalWhileCondition(const std::string& cond) {
        std::string c = trim(cond);
        std::vector<std::string> ops = { "<=", ">=", "==", "!=", "<", ">" };
        for (const auto& op : ops) {
            size_t pos = c.find(op);
            if (pos != std::string::npos) {
                std::string left = trim(c.substr(0, pos));
                std::string right = trim(c.substr(pos + op.size()));
                double lval = 0, rval = 0;
                if (fvariables.count(left)) {
                    scr_value v = fvariables[left];
                    if (v.type == ACS_NUMBER) lval = *(static_cast<int*>(v.value));
                    else if (v.type == ACS_FLOAT) lval = *(static_cast<float*>(v.value));
                } else if (variables.count(left)) {
                    scr_value v = variables[left];
                    if (v.type == ACS_NUMBER) lval = *(static_cast<int*>(v.value));
                    else if (v.type == ACS_FLOAT) lval = *(static_cast<float*>(v.value));
                } else {
                    lval = std::stod(left);
                }
                if (fvariables.count(right)) {
                    scr_value v = fvariables[right];
                    if (v.type == ACS_NUMBER) rval = *(static_cast<int*>(v.value));
                    else if (v.type == ACS_FLOAT) rval = *(static_cast<float*>(v.value));
                } else if (variables.count(right)) {
                    scr_value v = variables[right];
                    if (v.type == ACS_NUMBER) rval = *(static_cast<int*>(v.value));
                    else if (v.type == ACS_FLOAT) rval = *(static_cast<float*>(v.value));
                } else {
                    rval = std::stod(right);
                }
                if (op == "<") return lval < rval;
                if (op == ">") return lval > rval;
                if (op == "<=") return lval <= rval;
                if (op == ">=") return lval >= rval;
                if (op == "==") return lval == rval;
                if (op == "!=") return lval != rval;
            }
        }
        return false;
    }

    std::optional<scr_value> executeValue(const std::string& line, const std::string val_type)
    {
        //takes 2 args: command, parameters
        //can be either script variables or direct strings
        size_t startQuote = line.find('(');
        size_t endQuote = line.rfind(')');
        std::string src;
        std::string what;

        std::string content = line.substr(startQuote + 1, endQuote - startQuote - 1);
        // Split by first comma
        size_t commaPos = content.find(',');
        if (commaPos != std::string::npos) {
            src = trim(content.substr(0, commaPos));
            what = trim(content.substr(commaPos + 1));
        }
        else {
            src = trim(content);
            what = "";
        }
        if (src.size() >= 2 && src.front() == '"' && src.back() == '"') {
            src = src.substr(1, src.size() - 2);
        }
        else if (fvariables.count(src)) {
            scr_value v = fvariables[src];
            if (v.type == ACS_STRING) src = *(static_cast<std::string*>(v.value));
            else if (v.type == ACS_NUMBER) src = std::to_string(*(static_cast<int*>(v.value)));
            else if (v.type == ACS_FLOAT) src = std::to_string(*(static_cast<float*>(v.value)));
            else if (v.type == ACS_BOOLEAN) src = (*(static_cast<bool*>(v.value))) ? "true" : "false";
        }
        else if (variables.count(src)) {
            scr_value v = variables[src];
            if (v.type == ACS_STRING) src = *(static_cast<std::string*>(v.value));
            else if (v.type == ACS_NUMBER) src = std::to_string(*(static_cast<int*>(v.value)));
            else if (v.type == ACS_FLOAT) src = std::to_string(*(static_cast<float*>(v.value)));
            else if (v.type == ACS_BOOLEAN) src = (*(static_cast<bool*>(v.value))) ? "true" : "false";
        }
        if (what.size() >= 2 && what.front() == '"' && what.back() == '"') {
            what = what.substr(1, what.size() - 2);
        }
        else if (fvariables.count(what)) {
            scr_value v = fvariables[what];
            if (v.type == ACS_STRING) what = *(static_cast<std::string*>(v.value));
            else if (v.type == ACS_NUMBER) what = std::to_string(*(static_cast<int*>(v.value)));
            else if (v.type == ACS_FLOAT) what = std::to_string(*(static_cast<float*>(v.value)));
            else if (v.type == ACS_BOOLEAN) what = (*(static_cast<bool*>(v.value))) ? "true" : "false";
        }
        else if (variables.count(what)) {
            scr_value v = variables[what];
            if (v.type == ACS_STRING) what = *(static_cast<std::string*>(v.value));
            else if (v.type == ACS_NUMBER) what = std::to_string(*(static_cast<int*>(v.value)));
            else if (v.type == ACS_FLOAT) what = std::to_string(*(static_cast<float*>(v.value)));
            else if (v.type == ACS_BOOLEAN) what = (*(static_cast<bool*>(v.value))) ? "true" : "false";
        }
        {
            scr_value scal;
            if (src == "Env")
            {
                const char* env_p = std::getenv(what.c_str());
				std::string env = (env_p != nullptr) ? std::string(env_p) : "";
                if (!val_type.empty()) //for value attributions, the val_type string is empty
                {
                    if (val_type != "String" && env.empty())
                    {
                        scal.type = ACS_NONE;
                        scal.value = nullptr;
                        scal.size = 0;
                        return scal;
                    }
                }
                scal.type = ACS_STRING;
                scal.value = new std::string(env);
                scal.size = static_cast<int>(env.size());
                return scal;
            }
        }
        return std::nullopt;
    }

public:
    ScriptLauncher() = default;

    void parseScript(const std::string& script) {
        std::vector<std::string> lines;
        std::istringstream stream(script);
        std::string line;

        while (std::getline(stream, line)) {
            lines.push_back(line);
        }

        std::string currentFunction;

        for (size_t i = 0; i < lines.size(); i++) {
            std::string trimmedLine = trim(lines[i]);

            if (isCommentOrEmpty(trimmedLine)) continue;
            if (parseInclude(trimmedLine)) continue;
            if (parseVariable(trimmedLine, variables)) continue;
            if (parseFunction(trimmedLine, lines, i, currentFunction)) continue;
        }
        for (const auto& inc : includes) {
            if (inc != "None") {
                std::ifstream file(inc);
                if (!file) {
                    std::cerr << "Include not found: " << inc << std::endl;
                    continue;
                }
                std::stringstream buffer;
                buffer << file.rdbuf();
                parseScript(buffer.str()); // recursive parse
            }
        }
    }

    std::optional<scr_value> executeFunction(const std::string& funcName, const std::vector<std::string>& args, std::map<std::string, scr_value>& callerVars) {
        if (functions.find(funcName) == functions.end()) {
            std::cerr << "Function " << funcName << " not found" << std::endl;
            return std::nullopt;
        }

        fvariables.clear();

        const auto& inputParms = functions[funcName].inputParms;
        auto it = inputParms.begin();
        for (size_t i = 0; i < args.size() && it != inputParms.end(); ++i, ++it) {
            scr_value val;
            if (callerVars.find(args[i]) != callerVars.end()) {
                scr_value& src = callerVars[args[i]];
                val.type = src.type;
                val.size = src.size;
                switch (src.type) {
                case ACS_STRING: val.value = new std::string(*(static_cast<std::string*>(src.value))); break;
                case ACS_NUMBER: val.value = new int(*(static_cast<int*>(src.value))); break;
                case ACS_FLOAT: val.value = new float(*(static_cast<float*>(src.value))); break;
                case ACS_BOOLEAN: val.value = new bool(*(static_cast<bool*>(src.value))); break;
                default: val.value = nullptr;
                }
            }
            else if (variables.find(args[i]) != variables.end()) {
                scr_value& src = variables[args[i]];
                val.type = src.type;
                val.size = src.size;
                switch (src.type) {
                case ACS_STRING: val.value = new std::string(*(static_cast<std::string*>(src.value))); break;
                case ACS_NUMBER: val.value = new int(*(static_cast<int*>(src.value))); break;
                case ACS_FLOAT: val.value = new float(*(static_cast<float*>(src.value))); break;
                case ACS_BOOLEAN: val.value = new bool(*(static_cast<bool*>(src.value))); break;
                default: val.value = nullptr;
                }
            }
            else {
                initValue(val, args[i]);
            }
            fvariables[it->first] = val;
        }

        auto& lines = functions[funcName].body;
        for (size_t i = 0; i < lines.size(); ++i) {
            std::string line = trim(lines[i]);
            if (line.find("if(") == 0) {
                executeIf(lines, i);
                continue;
            }
            if (line.find("for(") == 0) { executeFor(lines, i); continue; }
            if (line.find("while(") == 0) { executeWhile(lines, i); continue; }
            if (line.find(printDef + "(") == 0) { executePrint(line); continue; }
            if (line.find(executeDef + "(") == 0) { executeSystem(line); continue; }
            //find if var attrib
            for (const auto& fvar : fvariables) {
                if (line.find(fvar.first) != std::string::npos) {
                    if (parseVarAttrib(line, fvar.first, fvariables)) {
                        continue;
                    }
                }
            }
            for (const auto& var : variables) {
                if (line.find(var.first) != std::string::npos) {
                    if (parseVarAttrib(line, var.first, variables)) {
                        continue;
                    }
                }
            }
            if (isCommentOrEmpty(line)) continue;
            if (parseFunctionCall(line, fvariables)) continue;
            if (parseVariable(line, fvariables)) continue;
            if (parseReturn(line)) break;
            if (line == "{") { executeBlock(lines, i); continue; }
        }

        if (variables.find("__return__") != variables.end()) {
            scr_value returnValue = variables["__return__"];
			if (returnValue.type == ACS_NONE) return std::nullopt;
			if (returnValue.value == nullptr) return std::nullopt;
			return returnValue;
        }

        return std::nullopt;
    }

    std::optional<scr_value> executeFunction(const std::string& funcName) {
        std::vector<std::string> emptyArgs;
        return executeFunction(funcName, emptyArgs, variables);
    }

    std::optional<scr_value> launch(const std::string& script) {
        parseScript(script);

        if (functions.find("Main") != functions.end()) {
            return executeFunction("Main");
        }

        return std::nullopt;
    }

    int runFileI(const std::string& filename) {
        std::ifstream f(filename);
		std::optional<scr_value> result;
        if (!f) return false;
        std::stringstream buf;
        buf << f.rdbuf();
        result = launch(buf.str());
        //the value of return will be 0 if string or float
        //numbers and boolean will return
        if (result.has_value())
        {
            scr_value v;
            if (v.type == ACS_NUMBER)
            {
                return *(static_cast<int*>(result->value));
            }
        }
        return 0;
    }

    bool runFileB(const std::string& filename) {
        std::ifstream f(filename);
        std::optional<scr_value> result;
        if (!f) return false;
        std::stringstream buf;
        buf << f.rdbuf();
        result = launch(buf.str());
        //the value of return will be 0 if string or float
        //numbers and boolean will return
        if (result.has_value())
        {
            scr_value v;
            if (v.type == ACS_BOOLEAN)
            {
                return *(static_cast<bool*>(result->value));
            }
        }
        return 0;
    }
};

int main() {
    //test for ACS script launcher
    std::string script = R"(
# work

@Include(None) #no include, just for parsing

#Variables function-wide
#String
String acs = "TestACS"
#Number
Number num = 42

#Function test
Fun AnotherFun(None) -> None
{
    Print("Inside AnotherFun")
    Return None
}

#Function test with return
Fun ReturnFun(None) -> Number
{
    Return 123
}

#Function test with param and return
Fun ReturnWithParam(Number val) -> Number
{
    Return val + 1
}

Fun Main(None) -> String
{
    String test = "This is a test"
    Print("Hello ACS %s %d %f %b", acs, num, 1.129420, True)
    Print("Test variable in func: %s", test)
    {
        String nested = "Nested variable"
        Print("Inside nested block: %s", nested)
    }

    if(true) {
        Print("Inside if block")
    }
    if(false) {
        Print("This won't print")
    }
    if(test == "This is a test") {
        Print("Condition met: %s", test)
        if (test != "a test") {
            Print("No there is no hello")
        }
    }
    for(i = 0 to 5; 1) {
        Print("For loop iteration: %d", i)
    }
    Number j = 0
    while(j < 3) {
        Print("While loop iteration: %d", j)
        j = j + 1
    }

    AnotherFun(None)
    Number retNum = ReturnFun(None)

    Print("ReturnFun returned: %d", retNum)

    Number a = 10
    a = a + 5
    Print("a after addition: %d", a)

    Float b = 5.5
    b = b * 2
    Print("b after multiplication: %f", b)

    String c = "Hello, "
    c = c + "World!"
    Print("c after concatenation: %s", c)

    String multi = "Hello, "
    multi = multi + "this is "
    multi = multi + "a multi-part "
    multi = multi + "string."
    Print("multi after concatenation: %s", multi)

    if(multi == "Hello, this is a multi-part string.") {
        Print("Multi-part string condition met")
    }
    else {
        Print("Multi-part string condition not met")
    }

    Number returnsParam = ReturnWithParam(10)
    Print("ReturnWithParam returned: %d", returnsParam)

    for(i = 0 to 5; 1) {
        if(i == 1)
        {
            Print("i is one")
        }
        elseif(i == 2)
        {
            Print("i is two")
        }
        else
        {
            Print("i is something else: %d", i)
        }
    }

    # no arg
    Execute("date")
    # with arg
    Execute("echo", "Hello from ACS")
    # arg is variable
    Execute("echo", acs)
    # get env var
    String path = GetValue(Env, "PATH")
    Print("PATH is: %s", path)
    path = GetValue(Env, "PROCESSOR_IDENTIFIER")
    Print("PROCESSOR_IDENTIFIER is: %s", path)
    Return "Function complete"
}
    )";

    ScriptLauncher launcher;
    auto result = launcher.launch(script);

    if (result.has_value()) {
        std::string ret;
        if (result->type == ACS_STRING) {
            ret = *(static_cast<std::string*>(result->value));
        }
        else if (result->type == ACS_NUMBER) {
            ret = std::to_string(*(static_cast<int*>(result->value)));
        }
        else if (result->type == ACS_FLOAT) {
            ret = std::to_string(*(static_cast<float*>(result->value)));
        }
        else if (result->type == ACS_BOOLEAN) {
            ret = (*(static_cast<bool*>(result->value))) ? "True" : "False";
        }
        else {
            ret = "None";
		}
        std::cout << "Script returned: " << ret << std::endl;
    }
    else {
        std::cout << "Script returned nothing" << std::endl;
    }

    return 0;
}